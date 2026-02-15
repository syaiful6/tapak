type t =
  { id : string option
  ; assigns : Context.t
  ; transport : string
  ; joined_topics : string list
  }

type connect_info =
  { request : Request.t
  ; params : Form.Urlencoded.t
  }

module type S = sig
  val connect : connect_info -> (Context.t, string) result
  val id : Context.t -> string option
end

let assigns t = t.assigns
let assign key value t = { t with assigns = Context.add key value t.assigns }
let find_assign key t = Context.find key t.assigns

let find_assign_exn key t =
  match find_assign key t with
  | Some value -> value
  | None -> failwith "Socket.find_assign_exn: key not found"

let id t = t.id
let transport t = t.transport
let joined_topics t = t.joined_topics
