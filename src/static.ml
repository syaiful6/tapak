open Imports
module Log = (val Logging.setup ~src:"tapak.static" ~doc:"Tapak static module")

module Piece : sig
  type t

  val of_string : string -> t option
  (** Smart constructor for a t. Wont allow unsafe components,
      such as empty strings, strings starting with '.', or
      strings containing '/' *)

  val of_list : string list -> t list option
  (** Smart constructor for a list of t. Fails if any component
      is unsafe *)

  val to_string : t -> string
  (** Convert a Piece.t back to string *)
end = struct
  type t = string

  let of_string s =
    if String.length s = 0
    then None
    else if String.get s 0 = '.'
    then None
    else match String.index_opt s '/' with Some _ -> None | _ -> Some s

  let of_list xs = Option.traverse_list of_string xs
  let to_string (s : t) = s
end

module Max_age = struct
  type t =
    [ `None
    | `Seconds of int
    | `Forever
    ]
end

type folder_name = Piece.t

type 'a folder_or_file =
  [ `Folder of folder_name * 'a folder_content
    (** Folder with its name and contents *)
  | `File of 'a
  ]

and 'a folder_content = 'a folder_or_file list
(** List of files and folders in a directory *)

type 'a optional_folder_or_file =
  [ 'a folder_or_file
  | `Missing
  ]

type lookup_error =
  [ `Permission_denied
  | `Not_found
  | `IO_error of string
  ]

type content_encoding =
  [ `Identity
  | `Gzip
  | `Br
  | `Deflate
  | `Zstd
  ]

module Finfo = struct
  type t =
    { name : string
    ; size : int64
    ; modified_time : Ptime.t
    ; encoding : content_encoding
      (** The encoding of the file. If `Identity, file is
            uncompressed. Otherwise, file is pre-compressed in
            the specified format. This is typically determined
            from the file extension or storage metadata. *)
    }
end

module type STORAGE = sig
  type t

  val available_encodings :
     Piece.t list
    -> (content_encoding list, lookup_error) result
  (** Query what content encodings are available for a given
      path. The backend decides how to discover encodings
      (e.g., by checking for .gz, .br files, or querying
      storage metadata). Returns empty list if path doesn't
      exist or only identity encoding exists. *)

  val lookup :
     ?encoding:content_encoding
    -> Piece.t list
    -> (t optional_folder_or_file, lookup_error) result
  (** Lookup a file or folder by its path components with
      optional encoding. If encoding is provided, backend
      should return the file in that encoding. Returns Ok with
      the file/folder/missing, or Error if lookup fails. Note:
      `Missing is returned for valid paths that don't exist,
      while errors are for I/O failures, permission issues,
      etc. *)

  val finfo : t -> Finfo.t option
  (** Get file metadata (name, size, modified time, encoding).
      Returns None for folders. *)

  val mime_type : t -> string
  (** Get the MIME type of the file *)

  val hash : t -> string option
  (** Optional hash of the file contents for ETag generation.
      Useful for conditional requests (If-None-Match). *)

  val content : sw:Eio.Switch.t -> t -> (Body.t, lookup_error) result
  (** Get the complete file content as a Body.t. The sw
      parameter is used to manage I/O resource lifecycle. The
      encoding information is provided via finfo. *)

  val partial_content :
     sw:Eio.Switch.t
    -> t
    -> start:int64
    -> end_:int64 option
    -> (Body.t, lookup_error) result
  (** Get a partial file content for Range requests. The sw
      parameter is used to manage I/O resource lifecycle.
      Returns the body slice from start to end (inclusive). *)
end

type config =
  { index_files : string list
    (** Files to serve when a directory is requested, e.g.,
          ["index.html"; "index.htm"] *)
  ; show_directory_listing : bool
    (** Whether to show directory listings when no index file
          is found *)
  ; max_age : Max_age.t  (** Cache control max-age setting *)
  ; cache_control : string option
    (** Custom Cache-Control header value, overrides max_age
          if set *)
  ; use_weak_etags : bool
    (** Whether to use weak ETags (W/"...") instead of strong
          ETags. Weak ETags indicate semantic equivalence but
          allow for minor differences (e.g., gzip vs
          uncompressed). Strong ETags require byte-for-byte
          equality. *)
  ; follow_symlinks : bool
    (** Whether to follow symbolic links (for filesystem
          backend) *)
  ; serve_hidden_files : bool
    (** Whether to serve files/directories starting with '.'
      *)
  }

let default_config =
  { index_files = [ "index.html"; "index.htm" ]
  ; show_directory_listing = false
  ; max_age = `None
  ; cache_control = None
  ; use_weak_etags = false
  ; follow_symlinks = false
  ; serve_hidden_files = false
  }

module Response_finfo : sig
  module ETag : sig
    type t =
      | Strong of string
      | Weak of string

    val parse : string -> t option
    val parse_list : string -> t list
    val value : t -> string
    val strong_compare : t -> t -> bool
    val weak_compare : t -> t -> bool
    val matches_any : use_weak_comparison:bool -> t -> t list -> bool
    val matches_wildcard : string -> bool
  end

  type response =
    { status : Piaf.Status.t
    ; offset : int64
    ; headers : Headers.t
    ; length : int64
    }

  type t =
    [ `Without_body of Piaf.Status.t
    | `With_body of response
    ]

  val conditional_request :
     request_headers:Headers.t
    -> response_headers:Headers.t
    -> Finfo.t
    -> t
end = struct
  module P = Header_parser

  type response =
    { status : Piaf.Status.t
    ; offset : int64
    ; headers : Headers.t
    ; length : int64
    }

  type t =
    [ `Without_body of Piaf.Status.t
    | `With_body of response
    ]

  let parse_http_date str = Http_date.parse str |> Result.to_option

  module ETag = struct
    type t =
      | Strong of string
      | Weak of string

    let parse etag_str =
      let len = String.length etag_str in
      if len >= 5 && String.sub etag_str ~pos:0 ~len:3 = "W/\""
      then
        (* Weak ETag: W/"value" *)
        if String.get etag_str (len - 1) = '"'
        then Some (Weak (String.sub etag_str ~pos:3 ~len:(len - 4)))
        else None
      else if len >= 2 && String.get etag_str 0 = '"'
      then
        (* Strong ETag: "value" *)
        if String.get etag_str (len - 1) = '"'
        then Some (Strong (String.sub etag_str ~pos:1 ~len:(len - 2)))
        else None
      else None

    let _to_string = function
      | Strong v -> "\"" ^ v ^ "\""
      | Weak v -> "W/\"" ^ v ^ "\""

    let _is_weak = function Weak _ -> true | Strong _ -> false
    let value = function Strong v | Weak v -> v

    let strong_compare t1 t2 =
      match t1, t2 with
      | Strong v1, Strong v2 -> String.equal v1 v2
      | _ -> false

    let weak_compare t1 t2 = String.equal (value t1) (value t2)

    let parse_list etag_header =
      String.split_on_char ~sep:',' etag_header
      |> List.filter_map (fun s ->
        let trimmed = String.trim s in
        parse trimmed)

    let matches_any ~use_weak_comparison etag etag_list =
      let compare_fn =
        if use_weak_comparison then weak_compare else strong_compare
      in
      List.exists (compare_fn etag) etag_list

    let matches_wildcard header_value =
      String.equal (String.trim header_value) "*"
  end

  let _if_modified_since headers =
    let open Option.Syntax in
    let* ms = Headers.get headers "If-Modified-Since" in
    parse_http_date ms

  let _if_unmodified_since headers =
    let open Option.Syntax in
    let* us = Headers.get headers "If-Unmodified-Since" in
    parse_http_date us

  let _if_match headers = Headers.get headers "If-Match"
  let _if_none_match headers = Headers.get headers "If-None-Match"

  let _if_range headers =
    let open Option.Syntax in
    let* ir = Headers.get headers "If-Range" in
    match String.get ir 0 with
    | '"' | 'W' ->
      (match ETag.parse ir with Some etag -> Some (`ETag etag) | None -> None)
    | _ ->
      (match parse_http_date ir with
      | Some date -> Some (`Date date)
      | None -> None)

  let check_range range size =
    match range with
    | P.Range.From beg -> beg, Int64.sub size 1L
    | P.Range.From_to (beg, end_) -> beg, Int64.min (Int64.sub size 1L) end_
    | P.Range.Suffix count ->
      let beg = Int64.max 0L (Int64.sub size count) in
      beg, Int64.sub size 1L

  let parse_range rn size =
    match P.Range.parse (Some rn) with
    | Error _ -> `Without_body `Range_not_satisfiable
    | Ok [] -> `Without_body `Range_not_satisfiable
    | Ok (r :: _) ->
      let beg, end_ = check_range r size in
      let length = Int64.add (Int64.sub end_ beg) 1L in
      let status =
        if beg = 0L && end_ = Int64.sub size 1L then `OK else `Partial_content
      in
      `With_body { status; headers = Headers.empty; offset = beg; length }

  let unconditional hm size =
    match Headers.get hm "Range" with
    | None ->
      `With_body
        { status = `OK; headers = Headers.empty; offset = 0L; length = size }
    | Some rn -> parse_range rn size

  let if_modified headers size mtime =
    let open Option.Syntax in
    let* ims = _if_modified_since headers in
    Option.some
      (if Http_date.equal mtime ims = false
       then
         `With_body
           { status = `OK; headers = Headers.empty; offset = 0L; length = size }
       else `Without_body `Not_modified)

  let if_unmodified headers size mtime =
    let open Option.Syntax in
    let* ius = _if_unmodified_since headers in
    Option.some
      (if Http_date.equal ius mtime
       then unconditional headers size
       else `Without_body `Precondition_failed)

  let if_match headers size etag_opt =
    let open Option.Syntax in
    let* if_match_header = _if_match headers in
    let* etag = etag_opt in
    Option.some
      (if ETag.matches_wildcard if_match_header
       then unconditional headers size
       else
         let etag_list = ETag.parse_list if_match_header in
         (* If-Match uses strong comparison *)
         if ETag.matches_any ~use_weak_comparison:false etag etag_list
         then unconditional headers size
         else `Without_body `Precondition_failed)

  let if_none_match headers size etag_opt =
    let open Option.Syntax in
    let* if_none_match_header = _if_none_match headers in
    let* etag = etag_opt in
    Option.some
      (if ETag.matches_wildcard if_none_match_header
       then `Without_body `Not_modified
       else
         let etag_list = ETag.parse_list if_none_match_header in
         (* If-None-Match uses weak comparison *)
         if ETag.matches_any ~use_weak_comparison:true etag etag_list
         then `Without_body `Not_modified
         else
           `With_body
             { status = `OK
             ; headers = Headers.empty
             ; offset = 0L
             ; length = size
             })

  let if_range headers size mtime etag_opt =
    let open Option.Syntax in
    let* ir = _if_range headers in
    match ir with
    | `ETag req_etag ->
      let* etag = etag_opt in
      Option.some
        (* If-Range uses strong comparison for ETags *)
        (if ETag.strong_compare req_etag etag
         then unconditional headers size
         else
           `With_body
             { status = `OK
             ; headers = Headers.empty
             ; offset = 0L
             ; length = size
             })
    | `Date req_date ->
      let* rng = Headers.get headers "Range" in
      Option.some
        (if Http_date.equal req_date mtime
         then parse_range rng size
         else
           `With_body
             { status = `OK
             ; headers = Headers.empty
             ; offset = 0L
             ; length = size
             })

  let content_range_header ~beg ~end_ total =
    Format.sprintf
      "bytes %s/%Ld"
      (if Int64.compare beg end_ = 0
       then "*"
       else Format.sprintf "%Ld-%Ld" beg end_)
      total

  let add_content_headers ~offset ~len ~size headers =
    let hs =
      Headers.add_unless_exists headers "Content-Length" (Int64.to_string len)
      |> fun hs -> Headers.add_unless_exists hs "Accept-Ranges" "bytes"
    in
    if Int64.compare len size = 0
    then hs
    else
      let end_ = Int64.add (Int64.sub offset 1L) len in
      Headers.add
        hs
        "Content-Range"
        (content_range_header ~beg:offset ~end_ size)

  let conditional_request ~request_headers ~response_headers finfo =
    let etag_opt =
      match Headers.get response_headers "ETag" with
      | None -> None
      | Some etag_str -> ETag.parse etag_str
    in
    let modified_time =
      match Http_date.of_ptime finfo.Finfo.modified_time with
      | Ok pt -> pt
      | Error _ -> Result.get_ok (Http_date.of_ptime Ptime.epoch)
    in

    (* RFC 7232 precedence order for conditional requests: 1. If-Match (if
       present, must match for request to proceed) 2. If-Unmodified-Since (if no
       If-Match) 3. If-None-Match (if present, must NOT match to proceed) 4.
       If-Modified-Since (if no If-None-Match) 5. If-Range (for range requests
       only) *)
    let mcondition =
      match if_match request_headers finfo.Finfo.size etag_opt with
      | Some res -> Some res
      | None ->
        (match if_unmodified request_headers finfo.Finfo.size modified_time with
        | Some res -> Some res
        | None ->
          (match if_none_match request_headers finfo.Finfo.size etag_opt with
          | Some res -> Some res
          | None ->
            (match
               if_modified request_headers finfo.Finfo.size modified_time
             with
            | Some res -> Some res
            | None ->
              if_range request_headers finfo.Finfo.size modified_time etag_opt)))
    in
    let condition =
      Option.value
        ~default:(unconditional request_headers finfo.Finfo.size)
        mcondition
    in
    match condition with
    | `Without_body status -> `Without_body status
    | `With_body { status; offset; length; _ } ->
      let rsp_headers =
        Headers.add_unless_exists
          response_headers
          "Last-Modified"
          (Format.asprintf "%a" Http_date.pp modified_time)
        |> fun hs ->
        add_content_headers ~offset ~len:length ~size:finfo.Finfo.size hs
      in
      `With_body { status; headers = rsp_headers; offset; length }
end

type fs_env = < clock : float Eio.Time.clock_ty Eio.Std.r >

let filesystem ~env:fs_env ?(follow = true) ?(ttl_seconds = 45.0) root =
  let open Eio in
  let ( / ) = Path.( / ) in
  let module File_system = struct
    type t =
      | File :
          { path : 'a Path.t
          ; stat : File.Stat.t
          }
          -> t

    module Path_key = struct
      type t = Piece.t list

      let equal p1 p2 =
        List.equal (fun a b -> Piece.to_string a = Piece.to_string b) p1 p2

      let hash pieces = Hashtbl.hash (List.map Piece.to_string pieces)
    end

    module Stat_key = struct
      type t = Piece.t list * content_encoding option

      let equal (p1, e1) (p2, e2) =
        let pieces_equal =
          List.equal (fun a b -> Piece.to_string a = Piece.to_string b) p1 p2
        in
        pieces_equal && e1 = e2

      let hash (pieces, encoding) =
        Hashtbl.hash (List.map Piece.to_string pieces, encoding)
    end

    module Stat_entry = struct
      type t = File.Stat.t

      let weight _ = 1
    end

    module Stat_cache = Lru.Make_ttl (Stat_key) (Stat_entry)

    module Encodings_entry = struct
      type t = content_encoding list

      let weight _ = 1
    end

    module Encodings_cache = Lru.Make_ttl (Path_key) (Encodings_entry)

    let stat_cache = Stat_cache.create ~ttl_seconds 512
    let encodings_cache = Encodings_cache.create ~ttl_seconds 512
    let get_current_time () = Eio.Time.now fs_env#clock

    let attempt_stat path =
      try
        let stat = Path.stat ~follow path in
        Ok stat
      with
      | Exn.Io (Fs.E (Not_found _), _) -> Error `Not_found
      | Exn.Io (Fs.E (Permission_denied _), _) -> Error `Permission_denied
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Error `Not_found
      | Unix.Unix_error (Unix.EACCES, _, _) -> Error `Permission_denied
      | e -> Error (`IO_error (Printexc.to_string e))

    let attempt_stat_cached pieces encoding path =
      let now = get_current_time () in
      let cache_key = pieces, encoding in
      match Stat_cache.find ~now cache_key stat_cache with
      | Some cached -> Ok cached
      | None ->
        (match attempt_stat path with
        | Ok stat ->
          Stat_cache.add ~now cache_key stat stat_cache;
          Ok stat
        | Error _ as e -> e)

    let of_path_name path =
      match Path.split path with
      | Some (_, basename) ->
        Piece.of_string basename
        |> Option.value ~default:(Piece.of_string "unknown" |> Option.get)
      | None -> Piece.of_string "root" |> Option.get

    let encoding_to_extension = function
      | `Identity -> ""
      | `Gzip -> ".gz"
      | `Br -> ".br"
      | `Deflate -> ".deflate"
      | `Zstd -> ".zst"

    let available_encodings pieces =
      let now = get_current_time () in
      match Encodings_cache.find ~now pieces encodings_cache with
      | Some cached -> Ok cached
      | None ->
        (try
           let base_path =
             List.fold_left (fun acc p -> acc / Piece.to_string p) root pieces
           in
           let encodings = ref [] in
           (match attempt_stat base_path with
           | Ok stat when stat.File.Stat.kind = `Regular_file ->
             encodings := `Identity :: !encodings
           | _ -> ());
           List.iter
             (fun enc ->
                let ext = encoding_to_extension enc in
                if ext <> ""
                then
                  let encoded_path =
                    match Path.split base_path with
                    | Some (dir, basename) -> dir / (basename ^ ext)
                    | None -> base_path
                  in
                  match attempt_stat encoded_path with
                  | Ok stat when stat.File.Stat.kind = `Regular_file ->
                    encodings := enc :: !encodings
                  | _ -> ())
             [ `Gzip; `Br; `Deflate; `Zstd ];
           Encodings_cache.add ~now pieces !encodings encodings_cache;
           Ok !encodings
         with
        | Exn.Io (Fs.E (Not_found _), _) -> Error `Not_found
        | Exn.Io (Fs.E (Permission_denied _), _) -> Error `Permission_denied
        | e -> Error (`IO_error (Printexc.to_string e)))

    let _lookup ?encoding pieces =
      let base_path =
        List.fold_left (fun acc p -> acc / Piece.to_string p) root pieces
      in
      let path =
        match encoding with
        | Some enc ->
          let ext = encoding_to_extension enc in
          if ext = ""
          then base_path
          else (
            match Path.split base_path with
            | Some (dir, basename) -> dir / (basename ^ ext)
            | None -> base_path)
        | None -> base_path
      in
      match attempt_stat_cached pieces encoding path with
      | Error _ as e -> e
      | Ok stat ->
        (match stat.File.Stat.kind with
        | `Regular_file -> Ok (`File (File { path; stat }))
        | `Directory ->
          let basename = of_path_name path in
          let contents =
            Path.read_dir path
            |> List.filter_map (fun rel ->
              match attempt_stat (path / rel) with
              | Ok rstat ->
                (match rstat.File.Stat.kind with
                | `Regular_file ->
                  Some (`File (File { path = path / rel; stat = rstat }))
                | `Directory ->
                  let folder_name = of_path_name (path / rel) in
                  Some (`Folder (folder_name, []))
                  (* Only one level listing *)
                | _ -> None)
              | Error _ -> None)
          in
          Ok (`Folder (basename, contents))
        | _ -> Ok `Missing)

    let lookup ?encoding pieces =
      try _lookup ?encoding pieces with
      | Exn.Io (Fs.E (Not_found _), _) -> Ok `Missing
      | Exn.Io (Fs.E (Permission_denied _), _) -> Error `Permission_denied
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Ok `Missing
      | Unix.Unix_error (Unix.EACCES, _, _) -> Error `Permission_denied
      | e -> Error (`IO_error (Printexc.to_string e))

    let get_extension path =
      let open Option.Syntax in
      let* _, basename = Path.split path in
      let* ix = String.rindex_opt basename '.' in
      Some (String.sub basename ~pos:ix ~len:(String.length basename - ix))

    let get_content_extension path =
      let open Option.Syntax in
      let* _, basename = Path.split path in
      let basename_no_compress =
        match String.rindex_opt basename '.' with
        | None -> basename
        | Some ix ->
          let ext =
            String.sub basename ~pos:ix ~len:(String.length basename - ix)
          in
          if List.mem ext [ ".gz"; ".br"; ".zst"; ".deflate" ]
          then String.sub basename ~pos:0 ~len:ix
          else basename
      in
      let* ix = String.rindex_opt basename_no_compress '.' in
      Some
        (String.sub
           basename_no_compress
           ~pos:ix
           ~len:(String.length basename_no_compress - ix))

    let finfo (File { path; stat }) =
      if stat.File.Stat.kind <> `Regular_file
      then None
      else
        let encoding =
          match get_extension path with
          | Some ".gz" -> `Gzip
          | Some ".br" -> `Br
          | Some ".deflate" -> `Deflate
          | Some ".zst" -> `Zstd
          | _ -> `Identity
        in
        let modified_time =
          match Ptime.of_float_s stat.File.Stat.mtime with
          | None -> Ptime.epoch
          | Some pt -> pt
        in
        Some
          { Finfo.name = of_path_name path |> Piece.to_string
          ; size = stat.File.Stat.size |> Optint.Int63.to_int64
          ; modified_time
          ; encoding
          }

    let mime_type (File { path; _ }) =
      (* For compressed files (e.g., style.css.gz), we need to detect the MIME
         type of the original content (style.css), not the compressed format.
         get_content_extension strips compression extensions before extracting
         the content extension. *)
      match Path.split path with
      | Some (_, basename) ->
        let filename_for_lookup =
          match get_content_extension path with
          | Some ext -> "file" ^ ext
          | None -> basename
        in
        Magic_mime.lookup
          ~default:"application/octet-stream"
          filename_for_lookup
      | None -> "application/octet-stream"

    let hash (File { stat; _ }) =
      Some
        (Format.asprintf
           "%Lx-%x"
           (stat.File.Stat.size |> Optint.Int63.to_int64)
           (Float.to_int stat.File.Stat.mtime))

    let content ~sw:_ (File { path; stat }) =
      try
        if stat.File.Stat.kind <> `Regular_file
        then Error `Not_found
        else
          match Path.native path with
          | None -> Error (`IO_error "Cannot get native path for file")
          | Some native_path ->
            let size = stat.File.Stat.size |> Optint.Int63.to_int64 in
            (match Body.sendfile ~length:(`Fixed size) native_path with
            | Ok body -> Ok body
            | Error (#Piaf.Error.common as e) ->
              Error (`IO_error (Format.asprintf "%a" Piaf.Error.pp_hum e)))
      with
      | Exn.Io (Fs.E (Not_found _), _) -> Error `Not_found
      | Exn.Io (Fs.E (Permission_denied _), _) -> Error `Permission_denied
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Error `Not_found
      | Unix.Unix_error (Unix.EACCES, _, _) -> Error `Permission_denied
      | e -> Error (`IO_error (Printexc.to_string e))

    let partial_content ~sw (File { path; stat }) ~start ~end_ =
      try
        if stat.File.Stat.kind <> `Regular_file
        then Error `Not_found
        else
          let file_size = stat.File.Stat.size |> Optint.Int63.to_int64 in
          let end_pos =
            match end_ with
            | None -> Int64.sub file_size 1L
            | Some e -> Int64.min e (Int64.sub file_size 1L)
          in
          let length = Int64.sub (Int64.add end_pos 1L) start in
          if length <= 0L || start >= file_size
          then Error `Not_found
          else
            let stream, stream_push = Piaf.Stream.create 4 in
            let buf_size = 8192 in
            let _producer_fiber =
              Eio.Fiber.fork_promise ~sw (fun () ->
                Path.with_open_in path (fun flow ->
                  let offset = ref start in
                  let remaining = ref length in
                  try
                    while !remaining > 0L do
                      let to_read =
                        Int.min buf_size (Int64.to_int !remaining)
                      in
                      let buf = Bigstringaf.create to_read in
                      let cstruct = Cstruct.of_bigarray buf ~len:to_read in
                      Eio.File.pread_exact
                        flow
                        ~file_offset:(Optint.Int63.of_int64 !offset)
                        [ cstruct ];
                      stream_push
                        (Some (Piaf.IOVec.make buf ~off:0 ~len:to_read));
                      offset := Int64.add !offset (Int64.of_int to_read);
                      remaining := Int64.sub !remaining (Int64.of_int to_read)
                    done;
                    stream_push None
                  with
                  | End_of_file -> stream_push None
                  | exn ->
                    stream_push None;
                    raise exn))
            in
            Ok (Body.of_stream ~length:(`Fixed length) stream)
      with
      | Exn.Io (Fs.E (Not_found _), _) -> Error `Not_found
      | Exn.Io (Fs.E (Permission_denied _), _) -> Error `Permission_denied
      | Unix.Unix_error (Unix.ENOENT, _, _) -> Error `Not_found
      | Unix.Unix_error (Unix.EACCES, _, _) -> Error `Permission_denied
      | End_of_file -> Error (`IO_error "Unexpected end of file")
      | e -> Error (`IO_error (Printexc.to_string e))
  end
  in
  (module File_system : STORAGE)

let make_etag ~use_weak_etags hash =
  match hash with
  | None -> None
  | Some h ->
    let etag_value =
      if use_weak_etags then "W/\"" ^ h ^ "\"" else "\"" ^ h ^ "\""
    in
    Some etag_value

let make_cache_control config =
  match config.cache_control with
  | Some cc -> cc
  | None ->
    (match config.max_age with
    | `None -> "no-cache"
    | `Seconds s -> Format.sprintf "public, max-age=%d" s
    | `Forever -> "public, max-age=31536000, immutable")

let encoding_to_string = function
  | `Identity -> None
  | `Gzip -> Some "gzip"
  | `Br -> Some "br"
  | `Deflate -> Some "deflate"
  | `Zstd -> Some "zstd"

let should_serve_file config name =
  if config.serve_hidden_files
  then true
  else String.length name = 0 || String.get name 0 <> '.'

let rec find_index_file :
  'a. (module STORAGE with type t = 'a) -> config -> Piece.t list -> 'a option
  =
 fun (type a) (module F : STORAGE with type t = a) config pieces ->
  match config.index_files with
  | [] -> None
  | index :: rest ->
    (match Piece.of_string index with
    | None ->
      find_index_file (module F) { config with index_files = rest } pieces
    | Some index_piece ->
      let index_path = pieces @ [ index_piece ] in
      (match F.lookup ~encoding:`Identity index_path with
      | Ok (`File file) -> Some file
      | Ok (`Folder _ | `Missing) | Error _ ->
        find_index_file (module F) { config with index_files = rest } pieces))

let serve (module F : STORAGE) ?(config = default_config) () request segments =
  let open Result.Syntax in
  let result =
    let* pieces =
      match
        Piece.of_list (List.filter (fun s -> String.length s > 0) segments)
      with
      | None -> Error (Response.create `Bad_request)
      | Some ps -> Ok ps
    in

    let available_encodings =
      match F.available_encodings pieces with
      | Ok encodings -> encodings
      | Error _ -> []
    in

    let accept_encoding_header = Request.header "Accept-Encoding" request in
    let preferred_encoding : content_encoding =
      match
        Header_parser.Content_negotiation.negotiate_encoding
          accept_encoding_header
          (available_encodings :> Header_parser.Accept.encoding list)
      with
      | Some (`Gzip as enc) -> enc
      | Some (`Br as enc) -> enc
      | Some (`Deflate as enc) -> enc
      | Some (`Zstd as enc) -> enc
      | Some (`Identity as enc) -> enc
      | Some (`Star | `Other _) | None -> `Identity
    in

    let* lookup_result =
      match F.lookup ~encoding:preferred_encoding pieces with
      | Error `Not_found -> Error (Response.create `Not_found)
      | Error `Permission_denied -> Error (Response.create `Forbidden)
      | Error (`IO_error msg) ->
        Log.err (fun m -> m "Static file IO error: %s" msg);
        Error (Response.create `Internal_server_error)
      | Ok result -> Ok result
    in

    let* file =
      match lookup_result with
      | `Missing -> Error (Response.create `Not_found)
      | `Folder (_name, _contents) ->
        (match find_index_file (module F) config pieces with
        | Some index_file -> Ok index_file
        | None ->
          (* TODO: Implement directory listing if
             config.show_directory_listing *)
          Error (Response.create `Forbidden))
      | `File file ->
        (match F.finfo file with
        | None -> Error (Response.create `Not_found)
        | Some finfo ->
          if not (should_serve_file config finfo.name)
          then Error (Response.create `Not_found)
          else Ok file)
    in

    let finfo =
      match F.finfo file with
      | Some fi -> fi
      | None -> failwith "unreachable: file should have finfo"
    in

    let mime_type = F.mime_type file in
    let etag = make_etag ~use_weak_etags:config.use_weak_etags (F.hash file) in
    let cache_control = make_cache_control config in

    let response_headers =
      Headers.empty |> fun hs ->
      Headers.add hs "Content-Type" mime_type |> fun hs ->
      Headers.add hs "Cache-Control" cache_control |> fun hs ->
      (match encoding_to_string finfo.encoding with
        | None -> hs
        | Some encoding -> Headers.add hs "Content-Encoding" encoding)
      |> fun hs ->
      match etag with
      | Some etag_value -> Headers.add hs "ETag" etag_value
      | None -> hs
    in

    let request_headers = Request.headers request in
    match
      Response_finfo.conditional_request
        ~request_headers
        ~response_headers
        finfo
    with
    | `Without_body status ->
      Ok (Response.create ~headers:response_headers status)
    | `With_body { status; headers; offset; length } ->
      let request_info = Request.info request in
      let sw_opt = request_info.sw in
      let handle_body () =
        match sw_opt with
        | Some sw ->
          if offset = 0L && length = finfo.size
          then F.content ~sw file
          else
            F.partial_content
              ~sw
              file
              ~start:offset
              ~end_:(Some (Int64.sub (Int64.add offset length) 1L))
        | None ->
          (* Fallback for tests: create a temporary switch. Note: This will
             block until content is fully read. *)
          Eio.Switch.run (fun sw ->
            if offset = 0L && length = finfo.size
            then F.content ~sw file
            else
              F.partial_content
                ~sw
                file
                ~start:offset
                ~end_:(Some (Int64.sub (Int64.add offset length) 1L)))
      in
      let body_result = handle_body () in
      (match body_result with
      | Error `Not_found -> Error (Response.create `Not_found)
      | Error `Permission_denied -> Error (Response.create `Forbidden)
      | Error (`IO_error msg) ->
        Log.err (fun m -> m "Static file content error: %s" msg);
        Error (Response.create `Internal_server_error)
      | Ok body -> Ok (Response.create ~headers ~body status))
  in
  match result with Ok response -> response | Error response -> response

let app (module F : STORAGE) ?config () request =
  let uri = Request.uri request in
  let path = Uri.path uri in
  let segments = String.split_on_char ~sep:'/' path in
  serve (module F) ?config () request segments
