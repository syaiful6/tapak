open Ctypes

module M (F : Ctypes.TYPE) = struct
  open F

  let simdutf_c_enum label typedef mapping =
    enum
      typedef
      ~typedef:true
      ~unexpected:(fun i ->
        invalid_arg (Printf.sprintf "Unsupported %s enum: %Ld" typedef i))
      (mapping
      |> List.map (fun (constructor, constant_name) ->
        ( constructor
        , constant ("TAPAK_SIMDUTF_" ^ label ^ "_" ^ constant_name) int64_t )))

  module Error_code = struct
    type t =
      [ `Success
      | `Header_bits
      | `Too_short
      | `Too_long
      | `Overlong
      | `Too_large
      | `Surrogate
      | `Invalid_base64_character
      | `Base64_input_remainder
      | `Base64_extra_bits
      | `Output_buffer_too_small
      | `Other
      ]

    let t : t typ =
      simdutf_c_enum
        "ERROR_CODE"
        "tapak_simdutf_error_code_t"
        [ `Success, "SUCCESS"
        ; `Header_bits, "HEADER_BITS"
        ; `Too_short, "TOO_SHORT"
        ; `Too_long, "TOO_LONG"
        ; `Overlong, "OVERLONG"
        ; `Too_large, "TOO_LARGE"
        ; `Surrogate, "SURROGATE"
        ; `Invalid_base64_character, "INVALID_BASE64_CHARACTER"
        ; `Base64_input_remainder, "BASE64_INPUT_REMAINDER"
        ; `Base64_extra_bits, "BASE64_EXTRA_BITS"
        ; `Output_buffer_too_small, "OUTPUT_BUFFER_TOO_SMALL"
        ; `Other, "OTHER"
        ]
  end

  module Result = struct
    type t

    let t : t structure typ = structure "tapak_simdutf_result_t"
    let t = typedef t "tapak_simdutf_result_t"
    let error = field t "error" Error_code.t
    let count = field t "count" size_t
    let () = seal t
  end

  module Base64 = struct
    type option =
      [ `Default
      | `Url
      | `Default_no_padding
      | `Url_with_padding
      | `Default_accept_garbage
      | `Url_accept_garbage
      | `Default_or_url
      | `Default_or_url_accept_garbage
      ]

    let option : option typ =
      simdutf_c_enum
        "BASE64_OPTIONS"
        "tapak_simdutf_base64_options_t"
        [ `Default, "DEFAULT"
        ; `Url, "URL"
        ; `Default_no_padding, "DEFAULT_NO_PADDING"
        ; `Url_with_padding, "URL_WITH_PADDING"
        ; `Default_accept_garbage, "DEFAULT_ACCEPT_GARBAGE"
        ; `Url_accept_garbage, "URL_ACCEPT_GARBAGE"
        ; `Default_or_url, "DEFAULT_OR_URL"
        ; `Default_or_url_accept_garbage, "DEFAULT_OR_URL_ACCEPT_GARBAGE"
        ]

    type last_chunk_option =
      [ `Loose
      | `Strict
      | `Stop_before_partial
      | `Only_full_chunks
      ]

    let last_chunk_option : last_chunk_option typ =
      simdutf_c_enum
        "BASE64_LCO"
        "tapak_simdutf_last_chunk_options_t"
        [ `Loose, "LOOSE"
        ; `Strict, "STRICT"
        ; `Stop_before_partial, "STOP_BEFORE_PARTIAL"
        ; `Only_full_chunks, "ONLY_FULL_CHUNKS"
        ]
  end
end
