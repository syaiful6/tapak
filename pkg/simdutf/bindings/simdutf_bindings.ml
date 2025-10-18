module M (F : Cstubs.FOREIGN) = struct
  let foreign = F.foreign

  module C = struct
    include Ctypes

    let ( @-> ) = F.( @-> )
    let returning = F.returning
  end

  module Error_code = struct
    let to_string =
      foreign
        "tapak_simdutf_error_to_string"
        C.(Simdutf_types.Error_code.t @-> returning string)
  end

  module Utf8 = struct
    let validate =
      foreign
        "tapak_simdutf_validate_utf8"
        C.(string @-> size_t @-> returning bool)

    let validate_with_errors =
      foreign
        "tapak_simdutf_validate_utf8_with_errors"
        C.(string @-> size_t @-> returning Simdutf_types.Result.t)
  end

  module Ascii = struct
    let validate =
      foreign
        "tapak_simdutf_validate_ascii"
        C.(string @-> size_t @-> returning bool)

    let validate_with_errors =
      foreign
        "tapak_simdutf_validate_ascii_with_errors"
        C.(string @-> size_t @-> returning Simdutf_types.Result.t)
  end

  module Base64 = struct
    let length_from_binary =
      foreign
        "tapak_simdutf_base64_length_from_binary"
        C.(size_t @-> Simdutf_types.Base64.option @-> returning size_t)

    let binary_to_base64 =
      foreign
        "tapak_simdutf_binary_to_base64"
        C.(
          ptr char
          @-> size_t
          @-> ptr char
          @-> Simdutf_types.Base64.option
          @-> returning size_t)

    let max_binary_length =
      foreign
        "tapak_simdutf_maximal_binary_length_from_base64"
        C.(ptr char @-> size_t @-> returning size_t)

    let base64_to_binary =
      foreign
        "tapak_simdutf_base64_to_binary"
        C.(
          ptr char
          @-> size_t
          @-> ptr char
          @-> Simdutf_types.Base64.option
          @-> Simdutf_types.Base64.last_chunk_option
          @-> returning Simdutf_types.Result.t)
  end
end
