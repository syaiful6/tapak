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

  let strip_prefix ~prefix s =
    let prefix_len = length prefix in
    if is_prefix s ~prefix
    then Some (sub s ~pos:prefix_len ~len:(length s - prefix_len))
    else None
end

module Option = struct
  include Option

  module Syntax = struct
    let ( <|> ) o1 o2 = match o1 with Some _ -> o1 | None -> o2
    let ( let+ ) result f = map f result
    let ( let* ) = bind

    let ( and* ) o1 o2 =
      match o1, o2 with Some v1, Some v2 -> Some (v1, v2) | _ -> None
  end

  let some_if cond v = if cond then Some v else None

  let traverse_list f xs =
    let rec aux acc = function
      | [] -> Some (List.rev acc)
      | x :: xs -> (match f x with None -> None | Some y -> aux (y :: acc) xs)
    in
    aux [] xs
end

module Result = struct
  include Result

  module Syntax = struct
    let ( let+ ) result f = map f result
    let ( let* ) = bind
    let ( <|> ) r1 r2 = match r1 with Ok _ -> r1 | Error _ -> r2

    let ( and+ ) r1 r2 =
      match r1, r2 with
      | Ok v1, Ok v2 -> Ok (v1, v2)
      | Error e, _ -> Error e
      | _, Error e -> Error e

    let ( and* ) = ( and+ )
  end
end
