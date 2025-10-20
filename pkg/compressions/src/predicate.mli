(** Predicates for deciding whether to compress HTTP responses.

    Predicates can be composed using boolean operators to create complex
    compression rules. *)

type t = Tapak_kernel.Request.t -> Tapak_kernel.Response.t -> bool
(** The type of a compression predicate. Takes a request and response,
    returns true if the response should be compressed. *)

(** {1 Predicate Combinators} *)

val ( && ) : t -> t -> t
(** Logical AND - both predicates must be true *)

val ( || ) : t -> t -> t
(** Logical OR - at least one predicate must be true *)

val not : t -> t
(** Logical NOT - negates the predicate *)

(** {1 Basic Predicates} *)

val always : t
(** Always compress *)

val never : t
(** Never compress *)

(** {1 Size-based Predicates} *)

val min_size : int -> t
(** Compress if content-length is above threshold.
    If Content-Length header is missing, defaults to true (compress).
    This is appropriate for streaming responses. *)

(** {1 Content-Type Predicates} *)

val content_type_matches : string list -> t
(** Compress only specific content types.
    Content-Type is compared case-insensitively, and parameters (like charset)
    are ignored. *)

val compressible_content_type : t
(** Compress common text-based content types:
    - text/html, text/plain, text/css, text/javascript, text/xml
    - application/json, application/javascript, application/xml
    - application/xhtml+xml, application/atom+xml, application/rss+xml
    - image/svg+xml *)

val not_already_compressed : t
(** Don't compress already-compressed content types like:
    - application/gzip, application/zip
    - image/jpeg, image/png, image/gif
    - video/mp4, audio/mpeg *)

(** {1 Header-based Predicates} *)

val not_already_encoded : t
(** Don't compress if response already has Content-Encoding header *)

val respect_no_transform : t
(** Don't compress if Cache-Control: no-transform is present (RFC 7234) *)

(** {1 Status-based Predicates} *)

val has_body : t
(** Don't compress certain status codes that don't have bodies
    (1xx, 204 No Content, 304 Not Modified) *)

(** {1 Default Predicate} *)

val default_predicate : t
(** Default predicate combining sensible defaults:
    - Minimum size: 32 bytes
    - Only compressible content types
    - Not already encoded
    - Respects Cache-Control: no-transform
    - Only responses with bodies

    Equivalent to:
    {[
      min_size 32
      && compressible_content_type
      && not_already_encoded
      && respect_no_transform
      && has_body
    ]} *)
