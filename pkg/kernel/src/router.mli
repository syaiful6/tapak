exception Not_found
exception Bad_request of string
exception Validation_failed of (string * string) list

type metadata =
  { operation_id : string option
  ; summary : string option
  ; description : string option
  ; tags : string list
  ; body_description : string option
  }

type (_, _) path =
  | Nil : ('a, 'a) path
  | Literal : string * ('a, 'b) path -> ('a, 'b) path
  | Capture :
      { parse : string -> 'param option
      ; format : 'param -> string
      ; type_name : string
      ; format_name : string option
      ; rest : ('a, 'b) path
      }
      -> ('param -> 'a, 'b) path
  | Enum :
      { parse : string -> 'param option
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
  | Response_model :
      { encoder : 'resp -> Response.t
      ; rest : ('a, Request.t -> 'resp) schema
      }
      -> ('a, Request.t -> Response.t) schema
  | Body :
      { input_type : 'input Schema.input
      ; schema : 'validated Schema.t
      ; rest : ('a, 'b) schema
      }
      -> ('validated -> 'a, 'b) schema
  | Guard :
      { guard : 'g Request_guard.t
      ; rest : ('a, 'b) schema
      }
      -> ('g -> 'a, 'b) schema
  | Meta :
      { meta : metadata
      ; rest : ('a, 'b) schema
      }
      -> ('a, 'b) schema

type route

val int : (int -> 'a, 'a) path
val int32 : (int32 -> 'a, 'a) path
val int64 : (int64 -> 'a, 'a) path
val str : (string -> 'a, 'a) path
val bool : (bool -> 'a, 'a) path
val splat : (string list -> 'a, 'a) path

val custom :
   parse:(string -> 'param option)
  -> format:('param -> string)
  -> type_name:string
  -> ?format_name:string
  -> unit
  -> ('param -> 'a, 'a) path

val enum :
   parse:(string -> 'param option)
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

val body :
   'input Schema.input
  -> 'validated Schema.t
  -> ('a, 'b) schema
  -> ('validated -> 'a, 'b) schema

val guard : 'g Request_guard.t -> ('a, 'b) schema -> ('g -> 'a, 'b) schema

val response_model :
   ('resp -> Response.t)
  -> ('a, Request.t -> 'resp) schema
  -> ('a, Request.t -> Response.t) schema

val into : 'a -> ('a, Request.t -> Response.t) schema -> route
val operation_id : string -> ('a, 'b) schema -> ('a, 'b) schema
val summary : string -> ('a, 'b) schema -> ('a, 'b) schema
val description : string -> ('a, 'b) schema -> ('a, 'b) schema
val tags : string list -> ('a, 'b) schema -> ('a, 'b) schema
val tag : string -> ('a, 'b) schema -> ('a, 'b) schema
val p : string -> ('a, 'b) path -> ('a, 'b) path
val ann : string * string -> ('a, 'b) path -> ('a, 'b) path
val match' : route list -> Request.t -> Response.t option
val router : route list -> Request.t -> Response.t
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
  val get : id -> Handler.t
  val edit : id -> Handler.t
  val update : id -> Handler.t
  val delete : id -> Handler.t
end

val resource :
   ?middlewares:Middleware.t list
  -> ('a, 'a) path
  -> (module Resource)
  -> route
