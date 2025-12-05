type t = (Request.t, Response.t) Filter.simple

module type Intf = sig
  type t

  val call : t -> (Request.t, Response.t) Filter.simple
end

val use : (module Intf with type t = 'a) -> 'a -> t
