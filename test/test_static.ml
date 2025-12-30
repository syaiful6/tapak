open Tapak

let make_finfo ~name ~size ~modified_time ?(encoding = `Identity) () =
  { Static.Finfo.name; size; modified_time; encoding }

let make_ptime year month day hour minute second =
  match
    Ptime.of_date_time ((year, month, day), ((hour, minute, second), 0))
  with
  | Some pt -> pt
  | None -> Ptime.epoch

let date1 = make_ptime 2024 1 1 12 0 0
let date2 = make_ptime 2024 1 2 12 0 0
let finfo1 = make_finfo ~name:"test.txt" ~size:1000L ~modified_time:date1 ()

let test_etag_parse_strong () =
  let module E = Static.Response_finfo.ETag in
  match E.parse "\"abc123\"" with
  | Some (E.Strong "abc123") -> ()
  | _ -> Alcotest.fail "Should parse strong ETag"

let test_etag_parse_weak () =
  let module E = Static.Response_finfo.ETag in
  match E.parse "W/\"abc123\"" with
  | Some (E.Weak "abc123") -> ()
  | _ -> Alcotest.fail "Should parse weak ETag"

let test_etag_parse_invalid () =
  let module E = Static.Response_finfo.ETag in
  match E.parse "invalid" with
  | None -> ()
  | Some _ -> Alcotest.fail "Should not parse invalid ETag"

let test_etag_parse_list () =
  let module E = Static.Response_finfo.ETag in
  let etags = E.parse_list "\"tag1\", W/\"tag2\", \"tag3\"" in
  Alcotest.(check int) "parse 3 etags" 3 (List.length etags)

let test_etag_strong_compare () =
  let module E = Static.Response_finfo.ETag in
  let etag1 = E.Strong "abc" in
  let etag2 = E.Strong "abc" in
  let etag3 = E.Weak "abc" in
  Alcotest.(check bool) "strong etags match" true (E.strong_compare etag1 etag2);
  Alcotest.(check bool)
    "strong vs weak don't match"
    false
    (E.strong_compare etag1 etag3)

let test_etag_weak_compare () =
  let module E = Static.Response_finfo.ETag in
  let etag1 = E.Strong "abc" in
  let etag2 = E.Weak "abc" in
  let etag3 = E.Weak "xyz" in
  Alcotest.(check bool)
    "weak comparison matches"
    true
    (E.weak_compare etag1 etag2);
  Alcotest.(check bool)
    "different values don't match"
    false
    (E.weak_compare etag1 etag3)

let test_unconditional_no_range () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.empty in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `OK; offset = 0L; length = 1000L; _ } -> ()
  | _ -> Alcotest.fail "Should return full content"

let test_unconditional_with_range () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "Range", "bytes=0-499" ] in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `Partial_content; offset = 0L; length = 500L; _ } ->
    ()
  | _ -> Alcotest.fail "Should return partial content"

let test_if_modified_since_not_modified () =
  let module R = Static.Response_finfo in
  let request_headers =
    Headers.of_list [ "If-Modified-Since", "Mon, 01 Jan 2024 12:00:00 GMT" ]
  in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `Without_body `Not_modified -> ()
  | _ -> Alcotest.fail "Should return 304 Not Modified"

let test_if_modified_since_modified () =
  let module R = Static.Response_finfo in
  let request_headers =
    Headers.of_list [ "If-Modified-Since", "Sun, 31 Dec 2023 12:00:00 GMT" ]
  in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `OK; _ } -> ()
  | _ -> Alcotest.fail "Should return 200 OK"

let test_if_unmodified_since_precondition_failed () =
  let module R = Static.Response_finfo in
  let request_headers =
    Headers.of_list [ "If-Unmodified-Since", "Sun, 31 Dec 2023 12:00:00 GMT" ]
  in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `Without_body `Precondition_failed -> ()
  | _ -> Alcotest.fail "Should return 412 Precondition Failed"

let test_if_unmodified_since_ok () =
  let module R = Static.Response_finfo in
  let request_headers =
    Headers.of_list [ "If-Unmodified-Since", "Mon, 01 Jan 2024 12:00:00 GMT" ]
  in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `OK; _ } -> ()
  | _ -> Alcotest.fail "Should return 200 OK"

let test_if_none_match_not_modified () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "If-None-Match", "\"abc123\"" ] in
  let response_headers = Headers.of_list [ "ETag", "\"abc123\"" ] in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `Without_body `Not_modified -> ()
  | _ -> Alcotest.fail "Should return 304 Not Modified"

let test_if_none_match_weak_etag () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "If-None-Match", "W/\"abc123\"" ] in
  let response_headers = Headers.of_list [ "ETag", "\"abc123\"" ] in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `Without_body `Not_modified -> ()
  | _ -> Alcotest.fail "Should match with weak comparison"

let test_if_none_match_no_match () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "If-None-Match", "\"xyz789\"" ] in
  let response_headers = Headers.of_list [ "ETag", "\"abc123\"" ] in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `OK; _ } -> ()
  | _ -> Alcotest.fail "Should return 200 OK when ETag doesn't match"

let test_if_none_match_wildcard () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "If-None-Match", "*" ] in
  let response_headers = Headers.of_list [ "ETag", "\"abc123\"" ] in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `Without_body `Not_modified -> ()
  | _ -> Alcotest.fail "Should match wildcard"

let test_if_match_ok () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "If-Match", "\"abc123\"" ] in
  let response_headers = Headers.of_list [ "ETag", "\"abc123\"" ] in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `OK; _ } -> ()
  | _ -> Alcotest.fail "Should return 200 OK when ETag matches"

let test_if_match_precondition_failed () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "If-Match", "\"xyz789\"" ] in
  let response_headers = Headers.of_list [ "ETag", "\"abc123\"" ] in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `Without_body `Precondition_failed -> ()
  | _ -> Alcotest.fail "Should return 412 when ETag doesn't match"

let test_if_match_weak_etag_fails () =
  let module R = Static.Response_finfo in
  (* If-Match requires strong comparison, so weak ETags should not match *)
  let request_headers = Headers.of_list [ "If-Match", "W/\"abc123\"" ] in
  let response_headers = Headers.of_list [ "ETag", "\"abc123\"" ] in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `Without_body `Precondition_failed -> ()
  | _ -> Alcotest.fail "If-Match should use strong comparison"

let test_if_match_wildcard () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "If-Match", "*" ] in
  let response_headers = Headers.of_list [ "ETag", "\"abc123\"" ] in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `OK; _ } -> ()
  | _ -> Alcotest.fail "Should match wildcard"

let test_if_range_etag_match () =
  let module R = Static.Response_finfo in
  let request_headers =
    Headers.of_list [ "Range", "bytes=100-199"; "If-Range", "\"abc123\"" ]
  in
  let response_headers = Headers.of_list [ "ETag", "\"abc123\"" ] in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `Partial_content; offset = 100L; length = 100L; _ } ->
    ()
  | _ -> Alcotest.fail "Should return partial content when ETag matches"

let test_if_range_etag_no_match () =
  let module R = Static.Response_finfo in
  let request_headers =
    Headers.of_list [ "Range", "bytes=100-199"; "If-Range", "\"xyz789\"" ]
  in
  let response_headers = Headers.of_list [ "ETag", "\"abc123\"" ] in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `OK; offset = 0L; length = 1000L; _ } -> ()
  | _ -> Alcotest.fail "Should return full content when ETag doesn't match"

let test_if_range_date_match () =
  let module R = Static.Response_finfo in
  let request_headers =
    Headers.of_list
      [ "Range", "bytes=100-199"; "If-Range", "Mon, 01 Jan 2024 12:00:00 GMT" ]
  in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `Partial_content; offset = 100L; length = 100L; _ } ->
    ()
  | _ -> Alcotest.fail "Should return partial content when date matches"

let test_if_range_date_no_match () =
  let module R = Static.Response_finfo in
  let request_headers =
    Headers.of_list
      [ "Range", "bytes=100-199"; "If-Range", "Sun, 31 Dec 2023 12:00:00 GMT" ]
  in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `OK; offset = 0L; length = 1000L; _ } -> ()
  | _ -> Alcotest.fail "Should return full content when date doesn't match"

let test_range_from () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "Range", "bytes=500-" ] in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `Partial_content; offset = 500L; length = 500L; _ } ->
    ()
  | _ -> Alcotest.fail "Should parse 'from' range"

let test_range_from_to () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "Range", "bytes=100-199" ] in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `Partial_content; offset = 100L; length = 100L; _ } ->
    ()
  | _ -> Alcotest.fail "Should parse 'from-to' range"

let test_range_suffix () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "Range", "bytes=-100" ] in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `Partial_content; offset = 900L; length = 100L; _ } ->
    ()
  | _ -> Alcotest.fail "Should parse suffix range"

let test_range_full_file () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "Range", "bytes=0-999" ] in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { status = `OK; offset = 0L; length = 1000L; _ } -> ()
  | _ -> Alcotest.fail "Should return 200 OK for full file range"

let test_range_invalid () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "Range", "bytes=invalid" ] in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `Without_body `Range_not_satisfiable -> ()
  | _ -> Alcotest.fail "Should return 416 for invalid range"

let test_precedence_if_match_overrides_if_modified () =
  let module R = Static.Response_finfo in
  let request_headers =
    Headers.of_list
      [ "If-Match", "\"xyz789\""
      ; "If-Modified-Since", "Sun, 31 Dec 2023 12:00:00 GMT"
      ]
  in
  let response_headers = Headers.of_list [ "ETag", "\"abc123\"" ] in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `Without_body `Precondition_failed -> ()
  | _ -> Alcotest.fail "If-Match should take precedence"

let test_precedence_if_none_match_overrides_if_modified () =
  let module R = Static.Response_finfo in
  let request_headers =
    Headers.of_list
      [ "If-None-Match", "\"abc123\""
      ; "If-Modified-Since", "Sun, 31 Dec 2023 12:00:00 GMT"
      ]
  in
  let response_headers = Headers.of_list [ "ETag", "\"abc123\"" ] in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `Without_body `Not_modified -> ()
  | _ -> Alcotest.fail "If-None-Match should take precedence"

let test_last_modified_header_added () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.empty in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { headers; _ } ->
    (match Headers.get headers "Last-Modified" with
    | Some "Mon, 01 Jan 2024 12:00:00 GMT" -> ()
    | _ -> Alcotest.fail "Should add Last-Modified header")
  | _ -> Alcotest.fail "Expected body response"

let test_content_length_header_added () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.empty in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { headers; _ } ->
    (match Headers.get headers "Content-Length" with
    | Some "1000" -> ()
    | _ -> Alcotest.fail "Should add Content-Length header")
  | _ -> Alcotest.fail "Expected body response"

let test_content_range_header_added () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.of_list [ "Range", "bytes=100-199" ] in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { headers; _ } ->
    (match Headers.get headers "Content-Range" with
    | Some "bytes 100-199/1000" -> ()
    | _ -> Alcotest.fail "Should add Content-Range header")
  | _ -> Alcotest.fail "Expected body response"

let test_accept_ranges_header_added () =
  let module R = Static.Response_finfo in
  let request_headers = Headers.empty in
  let response_headers = Headers.empty in
  match R.conditional_request ~request_headers ~response_headers finfo1 with
  | `With_body { headers; _ } ->
    (match Headers.get headers "Accept-Ranges" with
    | Some "bytes" -> ()
    | _ -> Alcotest.fail "Should add Accept-Ranges header")
  | _ -> Alcotest.fail "Expected body response"

module MockFile = struct
  type file_data =
    { name : string
    ; content : string
    ; modified_time : Ptime.t
    ; encoding : Static.content_encoding
    ; mime_type : string
    ; hash : string option
    }

  type t =
    | File of file_data
    | Folder of string * t list

  let files : (Static.Piece.t list * t) list ref = ref []
  let reset () = files := []

  let add_file
        ~path
        ~name
        ~content
        ~modified_time
        ?(encoding = `Identity)
        ?(mime_type = "text/plain")
        ?hash
        ()
    =
    let file_data =
      { name; content; modified_time; encoding; mime_type; hash }
    in
    files := (path, File file_data) :: !files

  let available_encodings pieces =
    (* Return encodings of files that match this path *)
    let encodings =
      List.filter_map
        (fun (p, f) ->
           if p = pieces
           then
             match f with File data -> Some data.encoding | Folder _ -> None
           else None)
        !files
    in
    Ok (if List.length encodings = 0 then [] else encodings)

  let lookup ?encoding pieces =
    (* If encoding is specified, try to find file with that encoding *)
    let matches =
      List.filter_map
        (fun (p, f) ->
           if p = pieces
           then
             match f, encoding with
             | File data, Some enc when data.encoding = enc -> Some f
             | File data, None when data.encoding = `Identity -> Some f
             | File _, None ->
               Some f (* Return any file if no encoding specified *)
             | _ -> None
           else None)
        !files
    in
    match matches with
    | [] ->
      (* Fallback to original behavior for backwards compat *)
      (match List.find_opt (fun (p, _) -> p = pieces) !files with
      | None -> Ok `Missing
      | Some ((_, File _data) as f) -> Ok (`File (snd f))
      | Some (_, Folder (name, contents)) ->
        (match Static.Piece.of_string name with
        | None -> Error `Not_found
        | Some piece ->
          Ok (`Folder (piece, List.map (fun t -> `File t) contents))))
    | file :: _ -> Ok (`File file)

  let finfo = function
    | File data ->
      Some
        { Static.Finfo.name = data.name
        ; size = Int64.of_int (String.length data.content)
        ; modified_time = data.modified_time
        ; encoding = data.encoding
        }
    | Folder _ -> None

  let mime_type = function
    | File data -> data.mime_type
    | Folder _ -> "application/octet-stream"

  let hash = function File data -> data.hash | Folder _ -> None

  let content ~sw = function
    | File data ->
      ignore sw;
      Ok (Piaf.Body.of_string data.content)
    | Folder _ -> Error `Not_found

  let partial_content ~sw t ~start ~end_ =
    ignore sw;
    match t with
    | File data ->
      let len = String.length data.content in
      let start_pos = Int64.to_int start in
      let end_pos =
        match end_ with Some e -> Int64.to_int e | None -> len - 1
      in
      let end_pos = min end_pos (len - 1) in
      if start_pos > end_pos || start_pos >= len
      then Error `Not_found
      else
        let slice_len = end_pos - start_pos + 1 in
        let slice = String.sub data.content start_pos slice_len in
        Ok (Piaf.Body.of_string slice)
    | Folder _ -> Error `Not_found
end

let make_request ?(headers = []) ?(meth = `GET) segments =
  let path = "/" ^ String.concat "/" segments in
  let headers = Headers.of_list headers in
  Request.create
    ~scheme:`HTTP
    ~version:Piaf.Versions.HTTP.HTTP_1_1
    ~headers
    ~meth
    ~body:Piaf.Body.empty
    path

let test_serve_simple_file () =
  Eio_main.run (fun _env ->
    MockFile.reset ();
    let path = Option.get (Static.Piece.of_list [ "test.txt" ]) in
    MockFile.add_file
      ~path
      ~name:"test.txt"
      ~content:"Hello, World!"
      ~modified_time:date1
      ();
    let request = make_request [ "test.txt" ] in
    let response = Static.serve (module MockFile) () request [ "test.txt" ] in
    Alcotest.(check int)
      "status 200"
      200
      (Response.status response |> Piaf.Status.to_code);
    match Response.body response |> Piaf.Body.to_string with
    | Ok body_str -> Alcotest.(check string) "body" "Hello, World!" body_str
    | Error _ -> Alcotest.fail "Failed to read body")

let test_serve_not_found () =
  Eio_main.run (fun _env ->
    MockFile.reset ();
    let request = make_request [ "missing.txt" ] in
    let response =
      Static.serve (module MockFile) () request [ "missing.txt" ]
    in
    Alcotest.(check int)
      "status 404"
      404
      (Response.status response |> Piaf.Status.to_code))

let test_serve_with_encoding () =
  Eio_main.run (fun _env ->
    MockFile.reset ();
    let path = Option.get (Static.Piece.of_list [ "style.css.gz" ]) in
    MockFile.add_file
      ~path
      ~name:"style.css.gz"
      ~content:"compressed-content"
      ~modified_time:date1
      ~encoding:`Gzip
      ~mime_type:"text/css"
      ();
    let request = make_request [ "style.css.gz" ] in
    let response =
      Static.serve (module MockFile) () request [ "style.css.gz" ]
    in
    Alcotest.(check int)
      "status 200"
      200
      (Response.status response |> Piaf.Status.to_code);
    let headers = Response.headers response in
    Alcotest.(check (option string))
      "Content-Encoding"
      (Some "gzip")
      (Headers.get headers "content-encoding");
    Alcotest.(check (option string))
      "Content-Type"
      (Some "text/css")
      (Headers.get headers "content-type"))

let test_serve_with_etag () =
  Eio_main.run (fun _env ->
    MockFile.reset ();
    let path = Option.get (Static.Piece.of_list [ "file.txt" ]) in
    MockFile.add_file
      ~path
      ~name:"file.txt"
      ~content:"content"
      ~modified_time:date1
      ~hash:"abc123"
      ();
    let request = make_request [ "file.txt" ] in
    let response = Static.serve (module MockFile) () request [ "file.txt" ] in
    let headers = Response.headers response in
    Alcotest.(check (option string))
      "ETag present"
      (Some "\"abc123\"")
      (Headers.get headers "etag"))

let test_serve_if_none_match () =
  Eio_main.run (fun _env ->
    MockFile.reset ();
    let path = Option.get (Static.Piece.of_list [ "file.txt" ]) in
    MockFile.add_file
      ~path
      ~name:"file.txt"
      ~content:"content"
      ~modified_time:date1
      ~hash:"abc123"
      ();
    let request =
      make_request ~headers:[ "If-None-Match", "\"abc123\"" ] [ "file.txt" ]
    in
    let response = Static.serve (module MockFile) () request [ "file.txt" ] in
    Alcotest.(check int)
      "status 304"
      304
      (Response.status response |> Piaf.Status.to_code))

let test_serve_range_request () =
  Eio_main.run (fun _env ->
    MockFile.reset ();
    let path = Option.get (Static.Piece.of_list [ "file.txt" ]) in
    MockFile.add_file
      ~path
      ~name:"file.txt"
      ~content:"0123456789"
      ~modified_time:date1
      ();
    let request =
      make_request ~headers:[ "Range", "bytes=2-5" ] [ "file.txt" ]
    in
    let response = Static.serve (module MockFile) () request [ "file.txt" ] in
    Alcotest.(check int)
      "status 206"
      206
      (Response.status response |> Piaf.Status.to_code);
    match Response.body response |> Piaf.Body.to_string with
    | Ok body_str -> Alcotest.(check string) "partial body" "2345" body_str
    | Error _ -> Alcotest.fail "Failed to read body")

let test_serve_nested_path () =
  Eio_main.run (fun _env ->
    MockFile.reset ();
    let path = Option.get (Static.Piece.of_list [ "css"; "style.css" ]) in
    MockFile.add_file
      ~path
      ~name:"style.css"
      ~content:"body { }"
      ~modified_time:date1
      ~mime_type:"text/css"
      ();
    let request = make_request [ "css"; "style.css" ] in
    let response =
      Static.serve (module MockFile) () request [ "css"; "style.css" ]
    in
    Alcotest.(check int)
      "status 200"
      200
      (Response.status response |> Piaf.Status.to_code);
    let headers = Response.headers response in
    Alcotest.(check (option string))
      "Content-Type"
      (Some "text/css")
      (Headers.get headers "content-type"))

let test_serve_empty_segments () =
  Eio_main.run (fun _env ->
    MockFile.reset ();
    let path = Option.get (Static.Piece.of_list []) in
    MockFile.add_file
      ~path
      ~name:"index.html"
      ~content:"<html></html>"
      ~modified_time:date1
      ~mime_type:"text/html"
      ();
    let request = make_request [] in
    let response = Static.serve (module MockFile) () request [] in
    (* With empty path, it serves the file at root *)
    Alcotest.(check int)
      "status"
      200
      (Response.status response |> Piaf.Status.to_code);
    match Response.body response |> Piaf.Body.to_string with
    | Ok body_str -> Alcotest.(check string) "body" "<html></html>" body_str
    | Error _ -> Alcotest.fail "Failed to read body")

let test_serve_cache_control () =
  Eio_main.run (fun _env ->
    MockFile.reset ();
    let path = Option.get (Static.Piece.of_list [ "file.txt" ]) in
    MockFile.add_file
      ~path
      ~name:"file.txt"
      ~content:"content"
      ~modified_time:date1
      ();
    let request = make_request [ "file.txt" ] in
    let config = { Static.default_config with max_age = `Seconds 3600 } in
    let response =
      Static.serve (module MockFile) ~config () request [ "file.txt" ]
    in
    let headers = Response.headers response in
    Alcotest.(check (option string))
      "Cache-Control"
      (Some "public, max-age=3600")
      (Headers.get headers "cache-control"))

let response_finfo_tests =
  ( "Static.Response_finfo"
  , [ Alcotest.test_case "ETag: parse strong" `Quick test_etag_parse_strong
    ; Alcotest.test_case "ETag: parse weak" `Quick test_etag_parse_weak
    ; Alcotest.test_case "ETag: parse invalid" `Quick test_etag_parse_invalid
    ; Alcotest.test_case "ETag: parse list" `Quick test_etag_parse_list
    ; Alcotest.test_case "ETag: strong compare" `Quick test_etag_strong_compare
    ; Alcotest.test_case "ETag: weak compare" `Quick test_etag_weak_compare
    ; Alcotest.test_case
        "Unconditional: no range"
        `Quick
        test_unconditional_no_range
    ; Alcotest.test_case
        "Unconditional: with range"
        `Quick
        test_unconditional_with_range
    ; Alcotest.test_case
        "If-Modified-Since: not modified"
        `Quick
        test_if_modified_since_not_modified
    ; Alcotest.test_case
        "If-Modified-Since: modified"
        `Quick
        test_if_modified_since_modified
    ; Alcotest.test_case
        "If-Unmodified-Since: precondition failed"
        `Quick
        test_if_unmodified_since_precondition_failed
    ; Alcotest.test_case
        "If-Unmodified-Since: ok"
        `Quick
        test_if_unmodified_since_ok
    ; Alcotest.test_case
        "If-None-Match: not modified"
        `Quick
        test_if_none_match_not_modified
    ; Alcotest.test_case
        "If-None-Match: weak etag"
        `Quick
        test_if_none_match_weak_etag
    ; Alcotest.test_case
        "If-None-Match: no match"
        `Quick
        test_if_none_match_no_match
    ; Alcotest.test_case
        "If-None-Match: wildcard"
        `Quick
        test_if_none_match_wildcard
    ; Alcotest.test_case "If-Match: ok" `Quick test_if_match_ok
    ; Alcotest.test_case
        "If-Match: precondition failed"
        `Quick
        test_if_match_precondition_failed
    ; Alcotest.test_case
        "If-Match: weak etag fails"
        `Quick
        test_if_match_weak_etag_fails
    ; Alcotest.test_case "If-Match: wildcard" `Quick test_if_match_wildcard
    ; Alcotest.test_case "If-Range: etag match" `Quick test_if_range_etag_match
    ; Alcotest.test_case
        "If-Range: etag no match"
        `Quick
        test_if_range_etag_no_match
    ; Alcotest.test_case "If-Range: date match" `Quick test_if_range_date_match
    ; Alcotest.test_case
        "If-Range: date no match"
        `Quick
        test_if_range_date_no_match
    ; Alcotest.test_case "Range: from" `Quick test_range_from
    ; Alcotest.test_case "Range: from-to" `Quick test_range_from_to
    ; Alcotest.test_case "Range: suffix" `Quick test_range_suffix
    ; Alcotest.test_case "Range: full file" `Quick test_range_full_file
    ; Alcotest.test_case "Range: invalid" `Quick test_range_invalid
    ; Alcotest.test_case
        "Precedence: If-Match overrides If-Modified-Since"
        `Quick
        test_precedence_if_match_overrides_if_modified
    ; Alcotest.test_case
        "Precedence: If-None-Match overrides If-Modified-Since"
        `Quick
        test_precedence_if_none_match_overrides_if_modified
    ; Alcotest.test_case
        "Headers: Last-Modified added"
        `Quick
        test_last_modified_header_added
    ; Alcotest.test_case
        "Headers: Content-Length added"
        `Quick
        test_content_length_header_added
    ; Alcotest.test_case
        "Headers: Content-Range added"
        `Quick
        test_content_range_header_added
    ; Alcotest.test_case
        "Headers: Accept-Ranges added"
        `Quick
        test_accept_ranges_header_added
    ] )

let serve_tests =
  ( "Static.serve"
  , [ Alcotest.test_case "Simple file" `Quick test_serve_simple_file
    ; Alcotest.test_case "Not found" `Quick test_serve_not_found
    ; Alcotest.test_case "With encoding (gzip)" `Quick test_serve_with_encoding
    ; Alcotest.test_case "With ETag" `Quick test_serve_with_etag
    ; Alcotest.test_case "If-None-Match 304" `Quick test_serve_if_none_match
    ; Alcotest.test_case "Range request 206" `Quick test_serve_range_request
    ; Alcotest.test_case "Nested path" `Quick test_serve_nested_path
    ; Alcotest.test_case "Empty segments" `Quick test_serve_empty_segments
    ; Alcotest.test_case "Cache control" `Quick test_serve_cache_control
    ] )

let tests = [ response_finfo_tests; serve_tests ]
