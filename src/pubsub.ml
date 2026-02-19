type message =
  { topic : string
  ; event : string
  ; payload : Jsont.json
  ; origin : string option
  }

type t =
  { subscribe : string -> (message -> unit) -> int
  ; unsubscribe : int -> unit
  ; broadcast_impl : message -> unit
  ; node_name : unit -> string
  }

module type S = sig
  type t
  type subscription

  val start : sw:Eio.Switch.t -> t
  val subscribe : t -> string -> (message -> unit) -> subscription
  val unsubscribe : t -> subscription -> unit
  val broadcast : t -> message -> unit
  val node_name : t -> string
end

let create : type a s.
  (module S with type t = a and type subscription = s) -> a -> t
  =
 fun (module B) backend ->
  let id = Atomic.make 0 in
  let subscriptions : (int, s * (message -> unit)) Saturn.Htbl.t =
    Saturn.Htbl.create ()
  in
  let subscribe topic callback =
    let backend_sub = B.subscribe backend topic callback in
    let sub_id = Atomic.fetch_and_add id 1 in
    (* not sure how to do if this fail *)
    let _ = Saturn.Htbl.try_add subscriptions sub_id (backend_sub, callback) in
    sub_id
  in
  let unsubscribe sub =
    match Saturn.Htbl.find_opt subscriptions sub with
    | Some (backend_sub, _) ->
      B.unsubscribe backend backend_sub;
      ignore (Saturn.Htbl.try_remove subscriptions sub)
    | None -> ()
  in
  let broadcast_impl msg = B.broadcast backend msg in
  { subscribe
  ; unsubscribe
  ; broadcast_impl
  ; node_name = (fun () -> B.node_name backend)
  }

let subscribe t = t.subscribe
let unsubscribe t = t.unsubscribe
let broadcast t msg = t.broadcast_impl msg
let node_name t = t.node_name ()

module Local : S with type subscription = int = struct
  type t =
    { next_id : int Atomic.t
    ; subscriptions : (int, string * (message -> unit)) Saturn.Htbl.t
    }

  type subscription = int

  let start ~sw:_ =
    { next_id = Atomic.make 0; subscriptions = Saturn.Htbl.create () }

  let subscribe t topic callback =
    let id = Atomic.fetch_and_add t.next_id 1 in
    ignore (Saturn.Htbl.try_add t.subscriptions id (topic, callback));
    id

  let unsubscribe t id = ignore @@ Saturn.Htbl.try_remove t.subscriptions id

  let broadcast t msg =
    Saturn.Htbl.to_seq t.subscriptions
    |> Seq.iter (fun (_, (topic, callback)) ->
      if String.equal topic msg.topic then callback msg)

  let node_name _ = "local"
end

let local ~sw =
  let backend = Local.start ~sw in
  create (module Local) backend
