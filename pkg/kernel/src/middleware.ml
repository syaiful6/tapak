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
  type args
  type state

  val init : args -> state
  val call : state -> (Request.t, Response.t) Filter.simple
end

let use (type a) ?(name = "") (module T : Intf with type args = a) (args : a) =
  let state = T.init args in
  { name; filter = T.call state }
