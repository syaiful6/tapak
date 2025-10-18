type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Error_code : sig
  type t = Simdutf_types.Error_code.t

  val to_string :
    (Simdutf_types.Error_code.t -> string Simdutf_ffi__.G.return)
      Simdutf_ffi__.G.result
end

module Utf8 : sig
  exception Validation_error of Error_code.t * int

  val validate : string -> bool
  val validate_with_errors : string -> (unit, Error_code.t * int) result
  val validate_exn : string -> unit
end

module Ascii : sig
  exception Validation_error of Error_code.t * int

  val validate : string -> bool
  val validate_with_errors : string -> (unit, Error_code.t * int) result
  val validate_exn : string -> unit
end

module Base64 : sig
  type option = Simdutf_types.Base64.option
  type last_chunk_option = Simdutf_types.Base64.last_chunk_option

  exception Decode_error of Error_code.t

  val encode :
     ?option:option
    -> ?offset:int
    -> ?len:int
    -> bigstring
    -> bigstring

  val decode :
     ?option:option
    -> ?last_chunk:last_chunk_option
    -> ?offset:int
    -> ?len:int
    -> bigstring
    -> (bigstring, Error_code.t) result

  val decode_exn :
     ?option:option
    -> ?last_chunk:last_chunk_option
    -> ?offset:int
    -> ?len:int
    -> bigstring
    -> bigstring
end
