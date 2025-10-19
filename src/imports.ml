module Logging = struct
  include Logs

  let setup ~src ~doc =
    let src = Src.create src ~doc in
    (module (val src_log src) : LOG)
end

module String = struct
  include StringLabels

  let rec check_prefix s ~prefix len i =
    i = len || (s.[i] = prefix.[i] && check_prefix s ~prefix len (i + 1))

  let is_prefix s ~prefix =
    let len = length s in
    let prefix_len = length prefix in
    len >= prefix_len && check_prefix s ~prefix prefix_len 0
end

module Result = struct
  include Result

  module Syntax = struct
    let ( let+ ) result f = map f result
    let ( let* ) = bind

    let ( and+ ) r1 r2 =
      match r1, r2 with
      | Ok v1, Ok v2 -> Ok (v1, v2)
      | Error e, _ -> Error e
      | _, Error e -> Error e

    let ( and* ) = ( and+ )
  end
end
