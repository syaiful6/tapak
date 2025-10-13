(** Middleware is a name, simple filter, that can be applied to a service. *)

type t =
  { filter : (Request.t, Response.t) Filter.simple
  ; name : string
  }

val create : filter:(Request.t, Response.t) Filter.simple -> name:string -> t

val apply :
   t
  -> (Request.t, Response.t) Service.t
  -> (Request.t, Response.t) Service.t

val apply_all :
   t list
  -> (Request.t, Response.t) Service.t
  -> (Request.t, Response.t) Service.t

module type Intf = sig
  type args
  type state

  val init : args -> state
  val call : state -> (Request.t, Response.t) Filter.simple
end

val use : ?name:string -> (module Intf with type args = 'a) -> 'a -> t
val pp : Format.formatter -> t -> unit
