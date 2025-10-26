type t =
  { filter : (Request.t, Response.t) Filter.simple
  ; name : string
  }

let create ~filter ~name = { filter; name }
let apply { filter; _ } handler = filter handler

let apply_all mws handler =
  let filters = List.map (fun mw -> mw.filter) mws in
  Filter.apply_all filters handler

let pp fmt { name; _ } = Format.fprintf fmt "Middleware(%s)" name

module type Intf = sig
  type t

  val call : t -> (Request.t, Response.t) Filter.simple
end

let use (type a) ~name (module M : Intf with type t = a) (args : a) =
  { name; filter = M.call args }
