type t =
  { id : string option
  ; assigns : Context.t
  ; transport : string
  ; joined_topics : string list
  }

type connect_info =
  { request : Request.t
  ; params : Form.Urlencoded.t
  }

module type S = sig
  val connect : connect_info -> (Context.t, string) result
  val id : Context.t -> string option
end

val assigns : t -> Context.t
val assign : 'a Context.key -> 'a -> t -> t
val find_assign : 'a Context.key -> t -> 'a option
val find_assign_exn : 'a Context.key -> t -> 'a
val id : t -> string option
val transport : t -> string
val joined_topics : t -> string list
