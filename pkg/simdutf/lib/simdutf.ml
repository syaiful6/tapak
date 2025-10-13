module Error_code = struct
  type t = Simdutf_types.Error_code.t

  let to_string = Simdutf_ffi.Error_code.to_string
end

module Utf8 = struct
  exception Validation_error of Error_code.t * int

  let validate input =
    let input_len = String.length input |> Unsigned.Size_t.of_int in
    let input_ptr = Ctypes.ocaml_string_start input in
    Simdutf_ffi.Utf8.validate input_ptr input_len

  let validate_with_errors input =
    let input_len = String.length input |> Unsigned.Size_t.of_int in
    let input_ptr = Ctypes.ocaml_string_start input in
    let result = Simdutf_ffi.Utf8.validate_with_errors input_ptr input_len in
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
    let input_len = String.length input |> Unsigned.Size_t.of_int in
    let input_ptr = Ctypes.ocaml_string_start input in
    Simdutf_ffi.Ascii.validate input_ptr input_len

  let validate_with_errors input =
    let input_len = String.length input |> Unsigned.Size_t.of_int in
    let input_ptr = Ctypes.ocaml_string_start input in
    let result = Simdutf_ffi.Ascii.validate_with_errors input_ptr input_len in
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

  let encode ?(option = `Default) input =
    let input_len = String.length input |> Unsigned.Size_t.of_int in
    let output_len = Simdutf_ffi.Base64.length_from_binary input_len option in
    let output_buffer =
      Ctypes.allocate_n Ctypes.char ~count:(Unsigned.Size_t.to_int output_len)
    in
    let input_ptr = Ctypes.ocaml_string_start input in
    let written =
      Simdutf_ffi.Base64.binary_to_base64
        input_ptr
        input_len
        output_buffer
        option
    in
    Ctypes.string_from_ptr
      output_buffer
      ~length:(Unsigned.Size_t.to_int written)

  let decode ?(option = `Default) ?(last_chunk = `Loose) input =
    let input_len = String.length input |> Unsigned.Size_t.of_int in
    let input_ptr = Ctypes.ocaml_string_start input in
    let max_output_len =
      Simdutf_ffi.Base64.max_binary_length input_ptr input_len
    in
    let output_buffer =
      Ctypes.allocate_n
        Ctypes.char
        ~count:(Unsigned.Size_t.to_int max_output_len)
    in
    let result =
      Simdutf_ffi.Base64.base64_to_binary
        input_ptr
        input_len
        output_buffer
        option
        last_chunk
    in
    let error = Ctypes.getf result Simdutf_types.Result.error in
    let count = Ctypes.getf result Simdutf_types.Result.count in
    match error with
    | `Success ->
      let output_str =
        Ctypes.string_from_ptr
          output_buffer
          ~length:(Unsigned.Size_t.to_int count)
      in
      Ok output_str
    | error_code -> Error error_code

  let decode_exn ?option ?last_chunk input =
    match decode ?option ?last_chunk input with
    | Ok s -> s
    | Error e -> raise (Decode_error e)
end
