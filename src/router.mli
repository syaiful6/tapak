exception Not_found
exception Bad_request of string
exception Validation_failed of (string * string) list

type extractor_error = ..

exception Extraction_failed of extractor_error

type 'a extractor = Request.t -> ('a, extractor_error) result

type media_type =
  | Json
  | Urlencoded
  | Multipart

type metadata =
  { operation_id : string option
  ; summary : string option
  ; description : string option
  ; tags : string list
  ; body_description : string option
  ; include_in_schema : bool
  }

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

type (_, _) schema =
  | Method : Piaf.Method.t * ('a, 'b) path -> ('a, 'b) schema
  | Query :
      { schema : 'query Sch.t
      ; rest : ('a, 'b) schema
      }
      -> ('query -> 'a, 'b) schema
  | Header :
      { schema : 'header Sch.t
      ; rest : ('a, 'b) schema
      }
      -> ('header -> 'a, 'b) schema
  | Cookie :
      { schema : 'cookie Sch.t
      ; rest : ('a, 'b) schema
      }
      -> ('cookie -> 'a, 'b) schema
  | Response_model :
      { schema : 'resp Sch.t
      ; status : Piaf.Status.t
      ; rest : ('a, 'resp) schema
      }
      -> ('a, Response.t) schema
  | Body :
      { input_type : media_type
      ; schema : 'validated Sch.t
      ; rest : ('a, 'b) schema
      }
      -> ('validated -> 'a, 'b) schema
  | Extract :
      { extractor : 'g extractor
      ; rest : ('a, 'b) schema
      }
      -> ('g -> 'a, 'b) schema
  | Meta :
      { meta : metadata
      ; rest : ('a, 'b) schema
      }
      -> ('a, 'b) schema

type route =
  | Route :
      { schema : ('a, Response.t) schema
      ; handler : 'a
      ; middlewares : Middleware.t list
      }
      -> route
  | Scope :
      { prefix : ('a, 'a) path
      ; routes : route list
      ; middlewares : Middleware.t list
      }
      -> route

val int : (int -> 'a, 'a) path
val int32 : (int32 -> 'a, 'a) path
val int64 : (int64 -> 'a, 'a) path
val str : (string -> 'a, 'a) path
val bool : (bool -> 'a, 'a) path
val splat : (string list -> 'a, 'a) path

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

val slug : (string -> 'a, 'a) path
val s : string -> ('a, 'a) path
val ( / ) : ('a, 'c) path -> ('c, 'b) path -> ('a, 'b) path
val get : ('a, 'b) path -> ('a, 'b) schema
val post : ('a, 'b) path -> ('a, 'b) schema
val put : ('a, 'b) path -> ('a, 'b) schema
val patch : ('a, 'b) path -> ('a, 'b) schema
val delete : ('a, 'b) path -> ('a, 'b) schema
val head : ('a, 'b) path -> ('a, 'b) schema
val any : ('a, 'b) path -> ('a, 'b) schema
val body : media_type -> 'a Sch.t -> ('b, 'c) schema -> ('a -> 'b, 'c) schema
val query : 'query Sch.t -> ('a, 'b) schema -> ('query -> 'a, 'b) schema
val header : 'a Sch.t -> ('b, 'c) schema -> ('a -> 'b, 'c) schema
val cookie : 'a Sch.t -> ('b, 'c) schema -> ('a -> 'b, 'c) schema
val extract : 'g extractor -> ('a, 'b) schema -> ('g -> 'a, 'b) schema
val request : ('a, 'b) schema -> (Request.t -> 'a, 'b) schema
val unit : ('a, 'b) schema -> (unit -> 'a, 'b) schema

val response_model :
   status:Piaf.Status.t
  -> schema:'resp Sch.t
  -> ('a, 'resp) schema
  -> ('a, Response.t) schema

val into : 'a -> ('a, Response.t) schema -> route
val recover : (Request.t -> exn -> Response.t option) -> route -> route
val operation_id : string -> ('a, 'b) schema -> ('a, 'b) schema
val summary : string -> ('a, 'b) schema -> ('a, 'b) schema
val description : string -> ('a, 'b) schema -> ('a, 'b) schema
val tags : string list -> ('a, 'b) schema -> ('a, 'b) schema
val tag : string -> ('a, 'b) schema -> ('a, 'b) schema
val include_in_schema : bool -> ('a, 'b) schema -> ('a, 'b) schema
val p : string -> ('a, 'b) path -> ('a, 'b) path
val ann : string * string -> ('a, 'b) path -> ('a, 'b) path
val match' : route list -> Request.t -> Response.t option
val router : route list -> Handler.t
val sprintf : ('a, string) path -> 'a
val get_method : ('a, 'b) schema -> Piaf.Method.t

val scope :
   ?middlewares:Middleware.t list
  -> ('a, 'a) path
  -> route list
  -> route

module type Resource = sig
  type id

  val id_path : unit -> (id -> 'a, 'a) path
  val index : Handler.t
  val new_ : Handler.t
  val create : Handler.t
  val get : Request.t -> id -> Response.t
  val edit : Request.t -> id -> Response.t
  val update : Request.t -> id -> Response.t
  val delete : Request.t -> id -> Response.t
end

val resource :
   ?middlewares:Middleware.t list
  -> ('a, 'a) path
  -> (module Resource)
  -> route

val validation_error_to_json : (string * string) list -> Jsont.json

val routes : ?not_found:Handler.t -> route list -> Handler.t
(** [routes ~not_found routes] creates a handler that dispatches to the given
    routes. If no route matches, the [not_found] handler is called instead of
    raising an exception. Default [not_found] returns a 404 "Not Found" response. *)
