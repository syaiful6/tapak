type t = (Request.t, Response.t) Filter.simple

module type Intf = sig
  type t

  val call : t -> (Request.t, Response.t) Filter.simple
end

let use (type a) (module M : Intf with type t = a) (args : a) = M.call args
