(* An AVL tree keyed by strings, searchable by a slice of a larger string so the
   router can look up a path segment without copying it out of the target.

   Keys are ordered by length first, then byte by byte. Any total order works
   for a map, and this one rejects most mismatches with a single integer
   comparison. *)

type 'a t =
  | Leaf
  | Node of
      { h : int
      ; s : int
      ; k : string
      ; v : 'a
      ; l : 'a t
      ; r : 'a t
      }

let empty = Leaf
let is_empty = function Leaf -> true | Node _ -> false
let height = function Leaf -> 0 | Node { h; _ } -> h
let cardinal = function Leaf -> 0 | Node { s; _ } -> s

let rec compare_bytes str off key i len =
  if i >= len
  then 0
  else
    let c =
      Char.compare (String.unsafe_get str (off + i)) (String.unsafe_get key i)
    in
    if c <> 0 then c else compare_bytes str off key (i + 1) len

let compare_span str ~off ~len key =
  let key_len = String.length key in
  if len <> key_len
  then Int.compare len key_len
  else compare_bytes str off key 0 len

let create l k v r =
  Node
    { h = 1 + Int.max (height l) (height r)
    ; s = cardinal l + cardinal r + 1
    ; k
    ; v
    ; l
    ; r
    }

let balance l k v r =
  let hl = height l in
  let hr = height r in
  if hl > hr + 1
  then
    match l with
    | Node { l = ll; k = lk; v = lv; r = lr; _ } when height ll >= height lr ->
      create ll lk lv (create lr k v r)
    | Node
        { l = ll
        ; k = lk
        ; v = lv
        ; r = Node { l = lrl; k = lrk; v = lrv; r = lrr; _ }
        ; _
        } ->
      create (create ll lk lv lrl) lrk lrv (create lrr k v r)
    | _ -> assert false
  else if hr > hl + 1
  then
    match r with
    | Node { l = rl; k = rk; v = rv; r = rr; _ } when height rr >= height rl ->
      create (create l k v rl) rk rv rr
    | Node
        { l = Node { l = rll; k = rlk; v = rlv; r = rlr; _ }
        ; k = rk
        ; v = rv
        ; r = rr
        ; _
        } ->
      create (create l k v rll) rlk rlv (create rlr rk rv rr)
    | _ -> assert false
  else create l k v r

(* Replaces the value if [key] is already bound *)
let rec insert key value = function
  | Leaf -> create Leaf key value Leaf
  | Node ({ k; v; l; r; _ } as node) ->
    let c = compare_span key ~off:0 ~len:(String.length key) k in
    if c = 0
    then if v == value then Node node else Node { node with v = value }
    else if c < 0
    then balance (insert key value l) k v r
    else balance l k v (insert key value r)

let rec find_unchecked str off len = function
  | Leaf -> None
  | Node { k; v; l; r; _ } ->
    let c = compare_span str ~off ~len k in
    if c = 0
    then Some v
    else if c < 0
    then find_unchecked str off len l
    else find_unchecked str off len r

let find_span str ~off ~len t =
  if off < 0 || len < 0 || off > String.length str - len
  then invalid_arg "Path_map.find_span: slice out of bounds";
  find_unchecked str off len t

let find key t = find_span key ~off:0 ~len:(String.length key) t

let rec iter f = function
  | Leaf -> ()
  | Node { k; v; l; r; _ } ->
    iter f l;
    f k v;
    iter f r

let rec fold f t acc =
  match t with
  | Leaf -> acc
  | Node { k; v; l; r; _ } -> fold f r (f k v (fold f l acc))

let bindings t = fold (fun k v acc -> (k, v) :: acc) t [] |> List.rev
