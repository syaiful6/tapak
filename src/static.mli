module Piece : sig
  type t
  (** A safe path component that cannot contain '/', cannot
      start with '.', and cannot be empty. Used to prevent
      directory traversal attacks. *)

  val of_string : string -> t option
  (** [of_string s] creates a safe path component from [s].
      Returns [None] if [s] is empty, starts with '.', or
      contains '/'. *)

  val of_list : string list -> t list option
  (** [of_list xs] converts a list of strings to safe path
      components. Returns [None] if any component is unsafe. *)

  val to_string : t -> string
  (** [to_string piece] converts a path component back to
      string. *)
end

(** {1 Cache Control} *)

module Max_age : sig
  type t =
    [ `None  (** No caching (Cache-Control: no-cache) *)
    | `Seconds of int  (** Cache for specified seconds *)
    | `Forever
      (** Cache indefinitely (Cache-Control: public,
          max-age=31536000, immutable) *)
    ]
end

(** {1 Content Encoding} *)

type content_encoding =
  [ `Identity  (** No compression *)
  | `Gzip  (** Gzip compressed (.gz) *)
  | `Br  (** Brotli compressed (.br) *)
  | `Deflate  (** Deflate compressed *)
  | `Zstd  (** Zstandard compressed (.zst) *)
  ]
(** Content encoding type indicating whether file content is
    compressed.

    This is typically determined from:
    - File extension (e.g., [.gz], [.br], [.zst])
    - Storage metadata (e.g., S3 object metadata)
    - Embedded file configuration *)

(** {1 File Metadata} *)

module Finfo : sig
  type t =
    { name : string
    ; size : int64
    ; modified_time : Ptime.t
    ; encoding : content_encoding
      (** The encoding of the file content.

            - [`Identity] - File is uncompressed (e.g.,
              [style.css])
            - [`Gzip] - File is gzip-compressed (e.g.,
              [style.css.gz])
            - [`Br] - File is brotli-compressed (e.g.,
              [style.css.br])
            - Other encodings similarly

            This is determined when obtaining file metadata
            (during stat/HEAD operation) and should reflect the
            actual storage format of the file. *)
    }
  (** File information including name, size, modified time, and
      encoding.

      The [size] field should reflect the actual byte count of
      the file as stored:
      - For uncompressed files: the uncompressed size
      - For pre-compressed files: the compressed size

      This ensures the Content-Length header is set correctly.
  *)
end

(** {1 File System Types} *)

type folder_name = Piece.t
(** Name of a folder. *)

type 'a folder_or_file =
  [ `Folder of folder_name * 'a folder_content
    (** A folder with its name and contents *)
  | `File of 'a  (** A file *)
  ]
(** Either a folder (with contents) or a file. *)

and 'a folder_content = 'a folder_or_file list
(** List of files and folders in a directory. *)

type 'a optional_folder_or_file =
  [ 'a folder_or_file
  | `Missing  (** Path exists conceptually but points to nothing *)
  ]
(** Like [folder_or_file] but can also represent a missing
    entry. *)

type lookup_error =
  [ `Not_found  (** File or directory not found *)
  | `Permission_denied  (** Access denied *)
  | `IO_error of string  (** I/O error with description *)
  ]
(** Error type for file operations. *)

(** {1 Backend Interface} *)

(** Signature for storage backend implementations.

    Backends can be implemented for:
    - Local filesystem
    - S3 or other object storage
    - Embedded files (compiled into binary)
    - In-memory storage
    - Custom storage systems *)
module type STORAGE = sig
  type t
  (** The file type for this backend. *)

  val available_encodings :
     Piece.t list
    -> (content_encoding list, lookup_error) result
  (** [available_encodings path] queries what content encodings
      are available for the given path.

      The backend decides how to discover available encodings:
      - Filesystem backends can check for [.gz], [.br], etc.
        files
      - Embedded/in-memory backends can return [[`Identity]]
        only
      - Cloud storage backends can query object metadata

      Returns an empty list if the path doesn't exist. This is
      not an error since it's used for content negotiation
      before actual lookup. *)

  val lookup :
     ?encoding:content_encoding
    -> Piece.t list
    -> (t optional_folder_or_file, lookup_error) result
  (** [lookup ?encoding path] looks up a file or folder by its
      path components with optional encoding preference.

      If [encoding] is provided, the backend should return the
      file in that encoding. For example, with [~encoding:`Br]
      and path ["style.css"], a filesystem backend might look
      for ["style.css.br"].

      Returns [Ok (`File file)] if path points to a file,
      [Ok (`Folder (name, contents))] if path points to a
      folder, [Ok `Missing] if path is valid but doesn't exist,
      or [Error e] for I/O failures, permission issues, etc.

      Note: [`Missing] is returned for valid paths that don't
      exist, while errors are for operational failures. *)

  val finfo : t -> Finfo.t option
  (** [finfo file] returns file metadata (name, size, modified
      time, encoding).

      Returns [None] for folders.

      The encoding should be determined from:
      - File extension (e.g., [.gz] → [`Gzip], [.br] → [`Br])
      - Storage metadata (e.g., S3 Content-Encoding header)
      - Default to [`Identity] if no compression

      The size must match the actual stored file size:
      - For [style.css] with [`Identity]: uncompressed size
      - For [style.css.gz] with [`Gzip]: compressed size

      Note: When converting filesystem modification times to
      {!Header_parser.Date.t}, subsecond precision is
      automatically dropped to match HTTP date format. This
      ensures correct If-Modified-Since comparisons. *)

  val mime_type : t -> string
  (** [mime_type file] returns the MIME type of the file.

      Common values: ["text/html"], ["application/json"],
      ["image/png"], etc.

      For compressed files, return the MIME type of the
      {b original} content, not the compressed format. For
      example:
      - [style.css.gz] should return ["text/css"], not
        ["application/gzip"]
      - This allows browsers to handle the content correctly
        after decompression *)

  val hash : t -> string option
  (** [hash file] returns an optional hash of the file contents
      for ETag generation.

      The hash should be stable for the same content. Useful
      for conditional requests (If-None-Match). If [None],
      ETags won't be generated. *)

  val content : sw:Eio.Switch.t -> t -> (Body.t, lookup_error) result
  (** [content ~sw file] returns the complete file content as a
      Body.t.

      The content should be returned as-is from storage
      (compressed or uncompressed). The encoding information is
      provided separately via {!finfo}.

      The [sw] parameter is used to manage the lifecycle of any
      I/O resources (file handles, etc.) needed to stream the
      content.

      Example:
      - For [style.css] with encoding [`Identity]: return
        uncompressed CSS
      - For [style.css.gz] with encoding [`Gzip]: return the
        gzipped bytes *)

  val partial_content :
     sw:Eio.Switch.t
    -> t
    -> start:int64
    -> end_:int64 option
    -> (Body.t, lookup_error) result
  (** [partial_content ~sw file ~start ~end_] returns a slice
      of the file content for HTTP Range requests.

      - [sw] is used to manage I/O resource lifecycle
      - [start] is the byte offset to start from (0-indexed)
      - [end_] is the optional end byte offset (inclusive)

      The slice should be taken from the {b stored} content
      (compressed if the file is compressed). This allows
      efficient range serving of pre-compressed files.

      Used to implement 206 Partial Content responses. *)
end

(** {1 Configuration} *)

type config =
  { index_files : string list
    (** Files to serve when a directory is requested.
          Default: [["index.html"; "index.htm"]]

          When a directory is requested, the server will try
          each file in order until one is found. *)
  ; show_directory_listing : bool
    (** Whether to show directory listings when no index file
          is found. Default: [false]

          TODO: Currently not implemented - will return 403
          Forbidden. *)
  ; max_age : Max_age.t
    (** Cache control max-age setting. Default: [`None]

          Determines Cache-Control header value unless
          [cache_control] is explicitly set. *)
  ; cache_control : string option
    (** Custom Cache-Control header value, overrides
          [max_age] if set. Default: [None]

          Example:
          [Some "public, max-age=3600, must-revalidate"] *)
  ; use_weak_etags : bool
    (** Whether to use weak ETags (W/"...") instead of strong
          ETags. Default: [false]

          Weak ETags indicate semantic equivalence but allow
          for minor differences (e.g., gzip vs uncompressed).
          Strong ETags require byte-for-byte equality. *)
  ; follow_symlinks : bool
    (** Whether to follow symbolic links (for filesystem
          backend). Default: [false]

          Security consideration: Following symlinks can expose
          files outside the intended directory. *)
  ; serve_hidden_files : bool
    (** Whether to serve files/directories starting with '.'.
          Default: [false]

          Hidden files (like .htaccess, .env) are often
          sensitive and should not be publicly accessible. *)
  }

val default_config : config
(** Default configuration:
    - index_files = [["index.html"; "index.htm"]]
    - show_directory_listing = [false]
    - max_age = [`None]
    - cache_control = [None]
    - use_weak_etags = [false]
    - follow_symlinks = [false]
    - serve_hidden_files = [false] *)

(** {1 Conditional Request Handling} *)

module Response_finfo : sig
  (** ETag utilities for conditional requests. *)
  module ETag : sig
    (** ETag type supporting both weak and strong validators.
    *)
    type t =
      | Strong of string  (** Strong ETag: "abc123" *)
      | Weak of string  (** Weak ETag: W/"abc123" *)

    val parse : string -> t option
    (** [parse s] parses an ETag string.

        Examples:
        - ["\"abc123\""] -> [Some (Strong "abc123")]
        - ["W/\"abc123\""] -> [Some (Weak "abc123")]
        - Invalid format -> [None] *)

    val parse_list : string -> t list
    (** [parse_list s] parses a comma-separated list of ETags.

        Example: ["\"tag1\", W/\"tag2\", \"tag3\""] *)

    val value : t -> string
    (** [value etag] extracts the raw value from an ETag
        (without quotes or W/). *)

    val strong_compare : t -> t -> bool
    (** [strong_compare e1 e2] performs strong comparison. Both
        ETags must be strong and have the same value. *)

    val weak_compare : t -> t -> bool
    (** [weak_compare e1 e2] performs weak comparison. Values
        must match, but strength doesn't matter. *)

    val matches_any : use_weak_comparison:bool -> t -> t list -> bool
    (** [matches_any ~use_weak_comparison etag list] checks if
        [etag] matches any ETag in [list] using the specified
        comparison mode. *)

    val matches_wildcard : string -> bool
    (** [matches_wildcard s] checks if [s] is the wildcard
        ["*"]. *)
  end

  type response =
    { status : Piaf.Status.t
    ; offset : int64
    ; headers : Headers.t
    ; length : int64
    }
  (** Response metadata for conditional requests with body. *)

  type t =
    [ `Without_body of Piaf.Status.t
      (** Return status code without body (304 Not Modified,
          412 Precondition Failed, etc.) *)
    | `With_body of response  (** Return body with specified parameters *)
    ]
  (** Result of conditional request processing. *)

  val conditional_request :
     request_headers:Headers.t
    -> response_headers:Headers.t
    -> Finfo.t
    -> t
  (** [conditional_request ~request_headers ~response_headers
       finfo] processes conditional request headers according
      to RFC 7232.

      Handles the following headers in precedence order: 1.
      If-Match (if present, must match for request to proceed)
      2. If-Unmodified-Since (if no If-Match) 3. If-None-Match
      (if present, must NOT match to proceed) 4.
      If-Modified-Since (if no If-None-Match) 5. If-Range (for
      range requests only)

      Also processes Range header for partial content requests.

      Returns either:
      - [`Without_body status] for 304 Not Modified, 412
        Precondition Failed, etc.
      - [`With_body { status; offset; length; headers }] with
        response metadata

      The returned headers include:
      - Last-Modified (from finfo)
      - Content-Length (based on offset/length)
      - Content-Range (for partial content)
      - Accept-Ranges: bytes *)
end

(** {1 Main API} *)

val serve :
   (module STORAGE)
  -> ?config:config
  -> unit
  -> Request.t
  -> string list
  -> Response.t
(** [serve (module STORAGE) ~config () segments] creates a
    static file handler that accepts path segments directly.

    This function is designed to work seamlessly with the
    router's splat parameter, accepting a [string list]
    representing path components. *)

type fs_env = < clock : float Eio.Time.clock_ty Eio.Std.r >

val filesystem :
   env:fs_env
  -> ?follow:bool
  -> ?ttl_seconds:float
  -> _ Eio.Path.t
  -> (module STORAGE)
(** [filesystem ~follow root] creates a filesystem-based static
    file backend.

    Returns a first-class module implementing {!STORAGE} that
    serves files from the local filesystem. *)

val app : (module STORAGE) -> ?config:config -> unit -> Tapak_kernel.Handler.t
(** [app (module STORAGE) ~config ()] creates a standalone
    static file handler.

    Unlike {!serve}, this function extracts path segments from
    the request URI and is suitable for use as a standalone
    handler or when you want to handle path prefix stripping
    yourself using {!Tapak_kernel.Filter.strip_prefix}. *)
