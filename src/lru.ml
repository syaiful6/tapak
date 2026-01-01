module type Weighted = sig
  type t

  val weight : t -> int
end

let invalid_arg fmt = Format.ksprintf invalid_arg fmt

type 'a fmt = Format.formatter -> 'a -> unit

let pf = Format.fprintf

let pp_iter ?(sep = Format.pp_print_space) pp ppf i =
  let first = ref true in
  i @@ fun x ->
  (match !first with true -> first := false | _ -> sep ppf ());
  pp ppf x

let cap_makes_sense ~m ~f cap =
  if cap < 0 then invalid_arg "Lru.%s.%s: ~cap:%d" m f cap

module M = struct
  module Q = struct
    type 'a node =
      { value : 'a
      ; mutable next : 'a node option
      ; mutable prev : 'a node option
      }

    type 'a t =
      { mutable first : 'a node option
      ; mutable last : 'a node option
      }

    let detach t n =
      let np = n.prev
      and nn = n.next in
      (match np with
      | None -> t.first <- nn
      | Some x ->
        x.next <- nn;
        n.prev <- None);
      match nn with
      | None -> t.last <- np
      | Some x ->
        x.prev <- np;
        n.next <- None

    let append t n =
      let on = Some n in
      match t.last with
      | Some x as l ->
        x.next <- on;
        t.last <- on;
        n.prev <- l
      | None ->
        t.first <- on;
        t.last <- on

    let node x = { value = x; prev = None; next = None }
    let create () = { first = None; last = None }

    let iter f t =
      let rec go f = function
        | Some n ->
          f n.value;
          go f n.next
        | _ -> ()
      in
      go f t.first

    let fold f t z =
      let rec go f z = function
        | Some n -> go f (f n.value z) n.prev
        | _ -> z
      in
      go f z t.last
  end

  module type S = sig
    type t
    type k
    type v

    val create : ?random:bool -> ?initialSize:int -> int -> t
    val is_empty : t -> bool
    val size : t -> int
    val weight : t -> int
    val capacity : t -> int
    val resize : int -> t -> unit
    val trim : t -> unit
    val mem : k -> t -> bool
    val find : k -> t -> v option
    val promote : k -> t -> unit
    val add : k -> v -> t -> unit
    val remove : k -> t -> unit
    val lru : t -> (k * v) option
    val drop_lru : t -> unit
    val fold : (k -> v -> 'a -> 'a) -> 'a -> t -> 'a
    val iter : (k -> v -> unit) -> t -> unit
    val of_list : (k * v) list -> t
    val to_list : t -> (k * v) list
    val pp : ?pp_size:(int * int) fmt -> ?sep:unit fmt -> (k * v) fmt -> t fmt
    val pp_dump : k fmt -> v fmt -> t fmt
  end

  module Bake (HT : Hashtbl.SeededS) (V : Weighted) = struct
    type k = HT.key
    type v = V.t

    type t =
      { ht : (k * v) Q.node HT.t
      ; q : (k * v) Q.t
      ; mutable cap : int
      ; mutable w : int
      }

    let size t = HT.length t.ht
    let weight t = t.w
    let capacity t = t.cap
    let is_empty t = HT.length t.ht = 0
    let cap_makes_sense = cap_makes_sense ~m:"M"

    let create ?random ?initialSize cap =
      let hashSize = match initialSize with None -> cap | Some v -> v in
      cap_makes_sense ~f:"create" cap;
      { cap; w = 0; ht = HT.create ?random hashSize; q = Q.create () }

    let lru t = match t.q.Q.first with Some n -> Some n.Q.value | _ -> None

    let drop_lru t =
      match t.q.Q.first with
      | None -> ()
      | Some ({ Q.value = k, v; _ } as n) ->
        t.w <- t.w - V.weight v;
        HT.remove t.ht k;
        Q.detach t.q n

    let rec trim t =
      if weight t > t.cap
      then (
        drop_lru t;
        trim t)

    let resize cap t =
      cap_makes_sense ~f:"resize" cap;
      t.cap <- cap

    let remove k t =
      try
        let n = HT.find t.ht k in
        t.w <- t.w - (snd n.Q.value |> V.weight);
        HT.remove t.ht k;
        Q.detach t.q n
      with
      | Not_found -> ()

    let add k v t =
      remove k t;
      let n = Q.node (k, v) in
      t.w <- t.w + V.weight v;
      HT.add t.ht k n;
      Q.append t.q n

    let promote k t =
      try
        let n = HT.find t.ht k in
        Q.(
          detach t.q n;
          append t.q n)
      with
      | Not_found -> ()

    let find k t =
      try Some (snd (HT.find t.ht k).Q.value) with Not_found -> None

    let mem k t = HT.mem t.ht k
    let iter f t = Q.iter (fun (k, v) -> f k v) t.q
    let fold f z t = Q.fold (fun (k, v) a -> f k v a) t.q z
    let to_list t = Q.fold (fun x xs -> x :: xs) t.q []

    let of_list xs =
      let t = create 0 in
      List.iter (fun (k, v) -> add k v t) xs;
      resize (Q.fold (fun (_, v) w -> w + V.weight v) t.q 0) t;
      t

    let pp ?(pp_size = fun _ -> ignore) ?sep pp ppf t =
      pf ppf "@[%a@[%a@]@]" pp_size (t.w, t.cap) (pp_iter ?sep pp) (fun f ->
        Q.iter f t.q)

    let pp_dump ppk ppv ppf =
      let sep ppf () = pf ppf ";@ "
      and ppkv ppf (k, v) = pf ppf "(@[%a,@ %a@])" ppk k ppv v in
      pf ppf "of_list [%a]" (pp ~sep ppkv)
  end

  module SeededHash (H : Hashtbl.HashedType) = struct
    include H

    let seeded_hash = Hashtbl.seeded_hash
  end

  module Make (K : Hashtbl.HashedType) (V : Weighted) =
    Bake (Hashtbl.MakeSeeded (SeededHash (K))) (V)

  module MakeSeeded (K : Hashtbl.SeededHashedType) (V : Weighted) =
    Bake (Hashtbl.MakeSeeded (K)) (V)
end

module type TTL = sig
  type t
  type k
  type v

  val create : ?random:bool -> ?initialSize:int -> ttl_seconds:float -> int -> t
  val find : now:float -> k -> t -> v option
  val add : now:float -> k -> v -> t -> unit
  val mem : now:float -> k -> t -> bool
  val remove : k -> t -> unit
  val trim : t -> unit
  val clear_expired : now:float -> t -> unit
end

module Make_ttl (K : Hashtbl.HashedType) (V : Weighted) :
  TTL with type k = K.t and type v = V.t = struct
  module Entry = struct
    type t = V.t * float (* value * timestamp *)

    let weight (v, _) = V.weight v
  end

  module Cache = M.Make (K) (Entry)

  type k = K.t
  type v = V.t

  type t =
    { cache : Cache.t
    ; ttl : float
    }

  let create ?random ?initialSize ~ttl_seconds cap =
    { cache = Cache.create ?random ?initialSize cap; ttl = ttl_seconds }

  let is_expired ttl timestamp now = now -. timestamp > ttl

  let find ~now k t =
    match Cache.find k t.cache with
    | None -> None
    | Some (v, ts) ->
      if is_expired t.ttl ts now
      then (
        Cache.remove k t.cache;
        None)
      else (
        Cache.promote k t.cache;
        Some v)

  let add ~now k v t =
    Cache.add k (v, now) t.cache;
    Cache.trim t.cache

  let mem ~now k t = match find ~now k t with Some _ -> true | None -> false
  let remove k t = Cache.remove k t.cache
  let trim t = Cache.trim t.cache

  let clear_expired ~now t =
    let to_remove = ref [] in
    Cache.iter
      (fun k (_, ts) ->
         if is_expired t.ttl ts now then to_remove := k :: !to_remove)
      t.cache;
    List.iter (fun k -> Cache.remove k t.cache) !to_remove
end
