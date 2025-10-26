type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Error_code = struct
  type t = Simdutf_types.Error_code.t

  let to_string = Simdutf_ffi.Error_code.to_string
end

module Utf8 = struct
  exception Validation_error of Error_code.t * int

  let validate input =
    let len = String.length input |> Unsigned.Size_t.of_int in
    Simdutf_ffi.Utf8.validate input len

  let validate_with_errors input =
    let len = String.length input |> Unsigned.Size_t.of_int in
    let result = Simdutf_ffi.Utf8.validate_with_errors input len in
    let error = Ctypes.getf result Simdutf_types.Result.error in
    let position = Ctypes.getf result Simdutf_types.Result.count in
    match error with
    | `Success -> Ok ()
    | error_code -> Error (error_code, Unsigned.Size_t.to_int position)

  let validate_exn input =
    match validate_with_errors input with
    | Ok () -> ()
    | Error (error_code, position) ->
      raise (Validation_error (error_code, position))
end

module Ascii = struct
  exception Validation_error of Error_code.t * int

  let validate input =
    let len = String.length input |> Unsigned.Size_t.of_int in
    Simdutf_ffi.Ascii.validate input len

  let validate_with_errors input =
    let len = String.length input |> Unsigned.Size_t.of_int in
    let result = Simdutf_ffi.Ascii.validate_with_errors input len in
    let error = Ctypes.getf result Simdutf_types.Result.error in
    let position = Ctypes.getf result Simdutf_types.Result.count in
    match error with
    | `Success -> Ok ()
    | error_code -> Error (error_code, Unsigned.Size_t.to_int position)

  let validate_exn input =
    match validate_with_errors input with
    | Ok () -> ()
    | Error (error_code, position) ->
      raise (Validation_error (error_code, position))
end

module Base64 = struct
  type option = Simdutf_types.Base64.option
  type last_chunk_option = Simdutf_types.Base64.last_chunk_option

  exception Decode_error of Error_code.t

  let encode ?(option = `Default) ?(offset = 0) ?len input =
    let input_dim = Bigarray.Array1.dim input in
    let len = match len with Some l -> l | None -> input_dim - offset in

    if offset < 0 || len < 0 || offset + len > input_dim
    then invalid_arg "Simdutf.Base64.encode: invalid offset or length";

    let input_ptr = Ctypes.(bigarray_start array1 input +@ offset) in
    let input_len = Unsigned.Size_t.of_int len in

    let output_len = Simdutf_ffi.Base64.length_from_binary input_len option in
    let buffer =
      Bigarray.Array1.create
        Bigarray.char
        Bigarray.c_layout
        (Unsigned.Size_t.to_int output_len)
    in
    let written =
      Simdutf_ffi.Base64.binary_to_base64
        input_ptr
        input_len
        Ctypes.(bigarray_start array1 buffer)
        option
    in
    if written == output_len
    then buffer
    else Bigarray.Array1.sub buffer 0 (Unsigned.Size_t.to_int written)

  let decode
        ?(option = `Default)
        ?(last_chunk = `Loose)
        ?(offset = 0)
        ?len
        input
    =
    let input_dim = Bigarray.Array1.dim input in
    let len = match len with Some l -> l | None -> input_dim - offset in

    if offset < 0 || len < 0 || offset + len > input_dim
    then invalid_arg "Simdutf.Base64.decode: invalid offset or length";

    let input_ptr = Ctypes.(bigarray_start array1 input +@ offset) in
    let input_len = Unsigned.Size_t.of_int len in

    let max_output_len =
      Simdutf_ffi.Base64.max_binary_length input_ptr input_len
    in
    let buffer =
      Bigarray.Array1.create
        Bigarray.char
        Bigarray.c_layout
        (Unsigned.Size_t.to_int max_output_len)
    in
    let result =
      Simdutf_ffi.Base64.base64_to_binary
        input_ptr
        input_len
        Ctypes.(bigarray_start array1 buffer)
        option
        last_chunk
    in
    let error = Ctypes.getf result Simdutf_types.Result.error in
    let count = Ctypes.getf result Simdutf_types.Result.count in
    match error with
    | `Success ->
      if count == max_output_len
      then Ok buffer
      else Ok (Bigarray.Array1.sub buffer 0 (Unsigned.Size_t.to_int count))
    | error_code -> Error error_code

  let decode_exn ?option ?last_chunk ?(offset = 0) ?len input =
    match decode ?option ?last_chunk ~offset ?len input with
    | Ok s -> s
    | Error e -> raise (Decode_error e)
end
