type t =
  { off : int
  ; len : int
  }

let make ~off ~len = { off; len }
let[@inline] off t = t.off
let[@inline] len t = t.len

(** [equal str span str2] checks whether the substring of [str] defined by [span]
    is equal to [str2]. *)
let equal str span str2 =
  let slen = String.length str2 in
  if slen <> span.len
  then false
  else
    let rec loop i =
      if i = slen
      then true
      else if String.unsafe_get str (span.off + i) <> String.unsafe_get str2 i
      then false
      else loop (i + 1)
    in
    loop 0

let parse_int str span =
  if span.len = 0
  then None
  else
    let rec loop i acc =
      if i = span.off + span.len
      then Some acc
      else
        let c = String.unsafe_get str i in
        if c >= '0' && c <= '9'
        then loop (i + 1) ((acc * 10) + (Char.code c - 48))
        else None
    in
    if String.unsafe_get str span.off = '-'
    then
      if span.len = 1 then None else Option.map Int.neg (loop (span.off + 1) 0)
    else loop span.off 0

let parse_int32 str span =
  match parse_int str span with
  | Some i when i >= -2147483648 && i <= 2147483647 -> Some (Int32.of_int i)
  | _ -> None

let parse_int64 str span =
  if span.len = 0
  then None
  else
    let rec loop i acc =
      if i = span.off + span.len
      then Some acc
      else
        let c = String.unsafe_get str i in
        if c >= '0' && c <= '9'
        then
          loop
            (i + 1)
            (Int64.add (Int64.mul acc 10L) (Int64.of_int (Char.code c - 48)))
        else None
    in
    if String.unsafe_get str span.off = '-'
    then
      if span.len = 1
      then None
      else Option.map Int64.neg (loop (span.off + 1) 0L)
    else loop span.off 0L

let parse_bool str span =
  if equal str span "true"
  then Some true
  else if equal str span "false"
  then Some false
  else None
