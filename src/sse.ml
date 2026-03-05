module Event = struct
  type data =
    | Text : string -> data
    | Json : 'a Jsont.t * 'a -> data

  type t =
    { id : string option
    ; data : data option
    ; event : string option
    ; comment : string option
    ; retry : int option (* milliseconds *)
    }

  let pp fmt t =
    let pp_opt pp_v fmt = function None -> () | Some v -> pp_v fmt v in
    let pp_data fmt = function
      | Text s ->
        String.split_on_char '\n' s
        |> List.iter (fun line -> Format.fprintf fmt "data: %s\n" line)
      | Json (codec, data) ->
        Format.fprintf
          fmt
          "data: %s\n"
          (Jsont_bytesrw.encode_string codec data |> Result.get_ok)
    in
    pp_opt (fun fmt id -> Format.fprintf fmt "id: %s\n" id) fmt t.id;
    pp_opt pp_data fmt t.data;
    pp_opt (fun fmt event -> Format.fprintf fmt "event: %s\n" event) fmt t.event;
    pp_opt
      (fun fmt comment -> Format.fprintf fmt ": %s\n" comment)
      fmt
      t.comment;
    pp_opt (fun fmt retry -> Format.fprintf fmt "retry: %d\n" retry) fmt t.retry;
    Format.fprintf fmt "\n"

  let to_string t = Format.asprintf "%a" pp t

  let text ?id ?event ?comment ?retry data =
    { id; data = Some (Text data); event; comment; retry }

  let json ?id ?event ?comment ?retry codec data =
    { id; data = Some (Json (codec, data)); event; comment; retry }

  let comment text =
    { id = None; data = None; event = None; comment = Some text; retry = None }
end

let ( <.> ) f g x = f (g x)

let stream ?(version = `HTTP_1_1) ?headers f =
  let base_headers =
    [ "Content-Type", "text/event-stream"; "Cache-Control", "no-cache" ]
  in
  let headers =
    Headers.add_list
      (Option.value headers ~default:(Headers.init ()))
      (match version with
      | `HTTP_1_0 -> ("Connection", "keep-alive") :: base_headers
      | _ -> base_headers)
  in
  Response.stream ~version ~headers (fun write flush ->
    f (write <.> Event.to_string) flush)
