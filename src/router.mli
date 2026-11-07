(** Type-safe routing.

    A route pairs an {!endpoint} (method, path and documentation) with a
    {!schemaf}: an applicative description of what to read from the request. The
    schema produces the handler, so path captures become its last arguments:

    {[
      get (s "users" / int64)
      |> summary "Get a user"
      >>
      let+ page = query page_schema
      and+ _ = secured (bearer verify) in
      fun id -> show_user page id
    ]}

    The same value drives dispatch and OpenAPI generation. *)

(** {1 Errors} *)

exception Not_found
exception Bad_request of string
exception Validation_failed of (string * string) list

type extractor_error = ..

exception Extraction_failed of extractor_error

type 'a extractor = Request.t -> ('a, extractor_error) result

val validation_error_to_json : (string * string) list -> Jsont.json

(** {1 Paths} *)

type (_, _) path =
  | Nil : ('a, 'a) path
  | Literal : string * ('a, 'b) path -> ('a, 'b) path
  | Capture :
      { parse : off:int -> len:int -> string -> 'param option
      ; format : 'param -> string
      ; type_name : string
      ; format_name : string option
      ; rest : ('a, 'b) path
      }
      -> ('param -> 'a, 'b) path
  | Enum :
      { parse : off:int -> len:int -> string -> 'param option
      ; format : 'param -> string
      ; type_name : string
      ; format_name : string option
      ; values : 'param list
      ; rest : ('a, 'b) path
      }
      -> ('param -> 'a, 'b) path
  | Annotated :
      { segment : ('a, 'b) path
      ; name : string
      ; description : string option
      }
      -> ('a, 'b) path
  | Splat : ('a, 'b) path -> (string list -> 'a, 'b) path

val int : (int -> 'a, 'a) path
val int32 : (int32 -> 'a, 'a) path
val int64 : (int64 -> 'a, 'a) path
val str : (string -> 'a, 'a) path
val bool : (bool -> 'a, 'a) path
val splat : (string list -> 'a, 'a) path
val slug : (string -> 'a, 'a) path
val s : string -> ('a, 'a) path
val ( / ) : ('a, 'c) path -> ('c, 'b) path -> ('a, 'b) path

val custom :
   parse:(off:int -> len:int -> string -> 'param option)
  -> format:('param -> string)
  -> type_name:string
  -> ?format_name:string
  -> unit
  -> ('param -> 'a, 'a) path

val enum :
   parse:(off:int -> len:int -> string -> 'param option)
  -> format:('param -> string)
  -> type_name:string
  -> ?format_name:string
  -> values:'param list
  -> unit
  -> ('param -> 'a, 'a) path

val p : string -> ('a, 'b) path -> ('a, 'b) path
(** [p name path] names a capture in the generated OpenAPI document. *)

val ann : string * string -> ('a, 'b) path -> ('a, 'b) path
(** [ann (name, description) path] is {!p} with a description. *)

val sprintf : ('a, string) path -> 'a
(** [sprintf path] builds a URL from the same path value a route matches. *)

(** {1 Security} *)

module Security : sig
  type scheme =
    | Basic of { realm : string }
    | Bearer of { format : string option }
    | Api_key of
        { name : string
        ; in_ : Security_api_key.location
        }

  type reason =
    | Missing
    | Malformed
    | Invalid

  type extractor_error += Unauthorized of scheme * reason

  type 'a t
  (** Parsing and documentation come from one value, so a route can't document a
      scheme it doesn't check. *)

  val scheme : 'a t -> scheme

  val challenge : scheme -> string option
  (** The [WWW-Authenticate] value for a 401 response, if the scheme has one. *)
end

(** Verify functions answer "who is this?": [None] is a 401. Rejecting a known
    user is authorization, which belongs in {!refine} with its own error. *)

val basic :
   ?realm:string
  -> (username:string -> password:string -> 'a option)
  -> 'a Security.t

val bearer : ?format:string -> (string -> 'a option) -> 'a Security.t

val api_key :
   in_:Security_api_key.location
  -> string
  -> (string -> 'a option)
  -> 'a Security.t

(** {1 Schemas} *)

type media_type =
  | Json
  | Urlencoded
  | Multipart

module Schema : sig
  type schemak
  type 'a schemaf = (schemak, 'a) Sch.Free.t

  type _ t =
    | Query : 'query Sch.t -> 'query t
    | Header : 'header Sch.t -> 'header t
    | Cookie : 'cookie Sch.t -> 'cookie t
    | Body :
        { media_types : media_type list
        ; schema : 'v Sch.t
        }
        -> 'v t
    | Extract : 'a extractor -> 'a t
    | Secured :
        { name : string
        ; scheme : Security.scheme
        ; extractor : 'a extractor
        }
        -> 'a t
    | Refine :
        { source : 'a schemaf
        ; check : 'a -> ('b, extractor_error) result
        }
        -> 'b t
    | Alt :
        { left : 'a schemaf
        ; right : 'a schemaf
        }
        -> 'a t
    | Fail : extractor_error -> 'a t
    | Memo :
        { key : 'a Context.key
        ; source : 'a schemaf
        }
        -> 'a t

  val prj : ('a, schemak) Sch.Sig.app -> 'a t
end

type 'a schemaf = 'a Schema.schemaf

val pure : 'a -> 'a schemaf
val map : ('a -> 'b) -> 'a schemaf -> 'b schemaf
val void : 'a schemaf -> unit schemaf
val ( <*> ) : ('a -> 'b) schemaf -> 'a schemaf -> 'b schemaf
val ( let+ ) : 'a schemaf -> ('a -> 'b) -> 'b schemaf
val ( and+ ) : 'a schemaf -> 'b schemaf -> ('a * 'b) schemaf
val query : 'a Sch.t -> 'a schemaf
val header : 'a Sch.t -> 'a schemaf
val cookie : 'a Sch.t -> 'a schemaf
val body : media_type -> 'a Sch.t -> 'a schemaf

val body_of : media_type list -> 'a Sch.t -> 'a schemaf
(** Like {!body}, with the request's content type picking the decoder.

    @raise Invalid_argument if the list is empty. *)

val extract : 'a extractor -> 'a schemaf
val request : Request.t schemaf

val secured : ?name:string -> 'a Security.t -> 'a schemaf
(** [name] identifies the scheme in the OpenAPI document. *)

val refine : ('a -> ('b, extractor_error) result) -> 'a schemaf -> 'b schemaf
val fail : extractor_error -> 'a schemaf

val memo : 'a schemaf -> 'a schemaf
(** Runs the source at most once per request. Each call creates its own cache
    entry, so sharing means reusing the value. *)

val ( <|> ) : 'a schemaf -> 'a schemaf -> 'a schemaf
(** [left <|> right] runs [right] when [left] fails validation or extraction;
    other exceptions propagate. If both fail, [right]'s error is raised. Effects
    in a failed [left] aren't undone.

    A body stream can't be re-read, so a body isn't retryable. To accept several
    body shapes, use a union in a single {!Sch.t}.

    @raise Invalid_argument if both branches read the body. *)

(** {1 Endpoints} *)

type metadata =
  { operation_id : string option
  ; summary : string option
  ; description : string option
  ; tags : string list
  ; body_description : string option
  ; include_in_schema : bool
  }

type response_doc =
  | Response_doc :
      { status : Http.Status.t
      ; schema : 'resp Sch.t
      }
      -> response_doc

type ('f, 'res) endpoint =
  | Endpoint :
      { method_ : Http.Method.t
      ; path : ('f, 'r) path
      ; finish : 'r -> 'res
      ; gates : unit schemaf list
      ; response : response_doc option
      ; meta : metadata
      }
      -> ('f, 'res) endpoint

val get : ('f, 'r) path -> ('f, 'r) endpoint
val head : ('f, 'r) path -> ('f, 'r) endpoint
val post : ('f, 'r) path -> ('f, 'r) endpoint
val put : ('f, 'r) path -> ('f, 'r) endpoint
val patch : ('f, 'r) path -> ('f, 'r) endpoint
val delete : ('f, 'r) path -> ('f, 'r) endpoint
val any : ('f, 'r) path -> ('f, 'r) endpoint

val gate : 'a schemaf -> ('f, 'res) endpoint -> ('f, 'res) endpoint
(** Runs [source] before the route's schema and drops its value. *)

val response_model :
   status:Http.Status.t
  -> schema:'resp Sch.t
  -> ('f, 'resp) endpoint
  -> ('f, Response.t) endpoint

val operation_id : string -> ('f, 'res) endpoint -> ('f, 'res) endpoint
val summary : string -> ('f, 'res) endpoint -> ('f, 'res) endpoint
val description : string -> ('f, 'res) endpoint -> ('f, 'res) endpoint
val tags : string list -> ('f, 'res) endpoint -> ('f, 'res) endpoint
val tag : string -> ('f, 'res) endpoint -> ('f, 'res) endpoint
val include_in_schema : bool -> ('f, 'res) endpoint -> ('f, 'res) endpoint

(** {1 Routes} *)

type route =
  | Route :
      { endpoint : ('f, Response.t) endpoint
      ; schema : 'f schemaf
      ; middlewares : Middleware.t list
      }
      -> route
  | Scope :
      { prefix : ('a, 'a) path
      ; routes : route list
      ; gates : unit schemaf list
      ; middlewares : Middleware.t list
      }
      -> route

val handle : 'f schemaf -> ('f, Response.t) endpoint -> route

val ( >> ) : ('f, Response.t) endpoint -> 'f schemaf -> route
(** [endpoint >> schema] is [handle schema endpoint]. *)

val handle_request : (Request.t -> 'f) -> ('f, Response.t) endpoint -> route
(** Routes the request to a plain handler, called once per request. *)

val scope :
   ?middlewares:Middleware.t list
  -> ?gates:unit schemaf list
  -> ('a, 'a) path
  -> route list
  -> route

val recover : (Request.t -> exn -> Response.t option) -> route -> route

val with_gates : unit schemaf list -> 'a schemaf -> 'a schemaf
(** The schema a route runs: [gates] in order, then [schema]. *)

(** {1 Dispatch} *)

val match' :
   ?unauthorized:(Request.t -> Security.scheme -> Security.reason -> Response.t)
  -> route list
  -> Request.t
  -> Response.t option
(** [match' routes req] is the response of the matching route, if there is one. *)

val of_list :
   ?not_found:Handler.t
  -> ?unauthorized:
       (Request.t -> Security.scheme -> Security.reason -> Response.t)
  -> route list
  -> Handler.t
(** [of_list routes] dispatches to [routes]. [not_found] answers when no route
    matches and defaults to a 404. [unauthorized] answers when a {!secured}
    schema rejects the request and defaults to a bare 401; the scheme's
    [WWW-Authenticate] challenge is added unless the response already sets one. *)
