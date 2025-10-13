/* Implementation of the C API for simdutf library */

#include "simdutf_capi.h"
#include "simdutf.h"

/* ========================================================================
 * Helper function to convert error codes
 * ======================================================================== */

static tapak_simdutf_error_code_t
convert_error_code(simdutf::error_code error) {
  switch (error) {
  case simdutf::SUCCESS:
    return TAPAK_SIMDUTF_ERROR_CODE_SUCCESS;
  case simdutf::HEADER_BITS:
    return TAPAK_SIMDUTF_ERROR_CODE_HEADER_BITS;
  case simdutf::TOO_SHORT:
    return TAPAK_SIMDUTF_ERROR_CODE_TOO_SHORT;
  case simdutf::TOO_LONG:
    return TAPAK_SIMDUTF_ERROR_CODE_TOO_LONG;
  case simdutf::OVERLONG:
    return TAPAK_SIMDUTF_ERROR_CODE_OVERLONG;
  case simdutf::TOO_LARGE:
    return TAPAK_SIMDUTF_ERROR_CODE_TOO_LARGE;
  case simdutf::SURROGATE:
    return TAPAK_SIMDUTF_ERROR_CODE_SURROGATE;
  case simdutf::INVALID_BASE64_CHARACTER:
    return TAPAK_SIMDUTF_ERROR_CODE_INVALID_BASE64_CHARACTER;
  case simdutf::BASE64_INPUT_REMAINDER:
    return TAPAK_SIMDUTF_ERROR_CODE_BASE64_INPUT_REMAINDER;
  case simdutf::BASE64_EXTRA_BITS:
    return TAPAK_SIMDUTF_ERROR_CODE_BASE64_EXTRA_BITS;
  case simdutf::OUTPUT_BUFFER_TOO_SMALL:
    return TAPAK_SIMDUTF_ERROR_CODE_OUTPUT_BUFFER_TOO_SMALL;
  case simdutf::OTHER:
  default:
    return TAPAK_SIMDUTF_ERROR_CODE_OTHER;
  }
}

static tapak_simdutf_result_t convert_result(const simdutf::result &result) {
  tapak_simdutf_result_t c_result;
  c_result.error = convert_error_code(result.error);
  c_result.count = result.count;
  return c_result;
}

static simdutf::base64_options
convert_base64_options(tapak_simdutf_base64_options_t options) {
  switch (options) {
  case TAPAK_SIMDUTF_BASE64_OPTIONS_DEFAULT:
    return simdutf::base64_default;
  case TAPAK_SIMDUTF_BASE64_OPTIONS_URL:
    return simdutf::base64_url;
  case TAPAK_SIMDUTF_BASE64_OPTIONS_DEFAULT_NO_PADDING:
    return simdutf::base64_default_no_padding;
  case TAPAK_SIMDUTF_BASE64_OPTIONS_URL_WITH_PADDING:
    return simdutf::base64_url_with_padding;
  case TAPAK_SIMDUTF_BASE64_OPTIONS_DEFAULT_ACCEPT_GARBAGE:
    return simdutf::base64_default_accept_garbage;
  case TAPAK_SIMDUTF_BASE64_OPTIONS_URL_ACCEPT_GARBAGE:
    return simdutf::base64_url_accept_garbage;
  case TAPAK_SIMDUTF_BASE64_OPTIONS_DEFAULT_OR_URL:
    return simdutf::base64_default_or_url;
  case TAPAK_SIMDUTF_BASE64_OPTIONS_DEFAULT_OR_URL_ACCEPT_GARBAGE:
    return simdutf::base64_default_or_url_accept_garbage;
  default:
    return simdutf::base64_default;
  }
}

static simdutf::last_chunk_handling_options
convert_last_chunk_options(tapak_simdutf_last_chunk_options_t options) {
  switch (options) {
  case TAPAK_SIMDUTF_BASE64_LCO_LOOSE:
    return simdutf::loose;
  case TAPAK_SIMDUTF_BASE64_LCO_STRICT:
    return simdutf::strict;
  case TAPAK_SIMDUTF_BASE64_LCO_STOP_BEFORE_PARTIAL:
    return simdutf::stop_before_partial;
  case TAPAK_SIMDUTF_BASE64_LCO_ONLY_FULL_CHUNKS:
    return simdutf::only_full_chunks;
  default:
    return simdutf::loose;
  }
}

/* ========================================================================
 * UTF-8 Validation Functions
 * ======================================================================== */

bool tapak_simdutf_validate_utf8(const char *buf, size_t len) {
  return simdutf::validate_utf8(buf, len);
}

tapak_simdutf_result_t tapak_simdutf_validate_utf8_with_errors(const char *buf,
                                                               size_t len) {
  simdutf::result result = simdutf::validate_utf8_with_errors(buf, len);
  return convert_result(result);
}

/* ========================================================================
 * ASCII Validation Functions
 * ======================================================================== */

bool tapak_simdutf_validate_ascii(const char *buf, size_t len) {
  return simdutf::validate_ascii(buf, len);
}

tapak_simdutf_result_t tapak_simdutf_validate_ascii_with_errors(const char *buf,
                                                                size_t len) {
  simdutf::result result = simdutf::validate_ascii_with_errors(buf, len);
  return convert_result(result);
}

/* ========================================================================
 * UTF-16 Validation Functions
 * ======================================================================== */

bool tapak_simdutf_validate_utf16(const uint16_t *buf, size_t len) {
  return simdutf::validate_utf16(reinterpret_cast<const char16_t *>(buf), len);
}

bool tapak_simdutf_validate_utf16le(const uint16_t *buf, size_t len) {
  return simdutf::validate_utf16le(reinterpret_cast<const char16_t *>(buf),
                                   len);
}

bool tapak_simdutf_validate_utf16be(const uint16_t *buf, size_t len) {
  return simdutf::validate_utf16be(reinterpret_cast<const char16_t *>(buf),
                                   len);
}

tapak_simdutf_result_t
tapak_simdutf_validate_utf16_with_errors(const uint16_t *buf, size_t len) {
  simdutf::result result = simdutf::validate_utf16_with_errors(
      reinterpret_cast<const char16_t *>(buf), len);
  return convert_result(result);
}

tapak_simdutf_result_t
tapak_simdutf_validate_utf16le_with_errors(const uint16_t *buf, size_t len) {
  simdutf::result result = simdutf::validate_utf16le_with_errors(
      reinterpret_cast<const char16_t *>(buf), len);
  return convert_result(result);
}

tapak_simdutf_result_t
tapak_simdutf_validate_utf16be_with_errors(const uint16_t *buf, size_t len) {
  simdutf::result result = simdutf::validate_utf16be_with_errors(
      reinterpret_cast<const char16_t *>(buf), len);
  return convert_result(result);
}

/* ========================================================================
 * UTF-32 Validation Functions
 * ======================================================================== */

bool tapak_simdutf_validate_utf32(const uint32_t *buf, size_t len) {
  return simdutf::validate_utf32(reinterpret_cast<const char32_t *>(buf), len);
}

tapak_simdutf_result_t
tapak_simdutf_validate_utf32_with_errors(const uint32_t *buf, size_t len) {
  simdutf::result result = simdutf::validate_utf32_with_errors(
      reinterpret_cast<const char32_t *>(buf), len);
  return convert_result(result);
}

/* ========================================================================
 * Base64 Encoding Functions
 * ======================================================================== */

size_t tapak_simdutf_base64_length_from_binary(
    size_t length, tapak_simdutf_base64_options_t options) {
  simdutf::base64_options cpp_options = convert_base64_options(options);
  return simdutf::base64_length_from_binary(length, cpp_options);
}

size_t tapak_simdutf_binary_to_base64(const char *input, size_t length,
                                      char *output,
                                      tapak_simdutf_base64_options_t options) {
  simdutf::base64_options cpp_options = convert_base64_options(options);
  return simdutf::binary_to_base64(input, length, output, cpp_options);
}

/* ========================================================================
 * Base64 Decoding Functions
 * ======================================================================== */

size_t tapak_simdutf_maximal_binary_length_from_base64(const char *input,
                                                       size_t length) {
  return simdutf::maximal_binary_length_from_base64(input, length);
}

tapak_simdutf_result_t tapak_simdutf_base64_to_binary(
    const char *input, size_t length, char *output,
    tapak_simdutf_base64_options_t options,
    tapak_simdutf_last_chunk_options_t last_chunk_options) {
  simdutf::base64_options cpp_options = convert_base64_options(options);
  simdutf::last_chunk_handling_options cpp_last_chunk =
      convert_last_chunk_options(last_chunk_options);
  simdutf::result result = simdutf::base64_to_binary(
      input, length, output, cpp_options, cpp_last_chunk);
  return convert_result(result);
}

size_t
tapak_simdutf_maximal_binary_length_from_base64_u16(const uint16_t *input,
                                                    size_t length) {
  return simdutf::maximal_binary_length_from_base64(
      reinterpret_cast<const char16_t *>(input), length);
}

tapak_simdutf_result_t tapak_simdutf_base64_to_binary_u16(
    const uint16_t *input, size_t length, char *output,
    tapak_simdutf_base64_options_t options,
    tapak_simdutf_last_chunk_options_t last_chunk_options) {
  simdutf::base64_options cpp_options = convert_base64_options(options);
  simdutf::last_chunk_handling_options cpp_last_chunk =
      convert_last_chunk_options(last_chunk_options);
  simdutf::result result =
      simdutf::base64_to_binary(reinterpret_cast<const char16_t *>(input),
                                length, output, cpp_options, cpp_last_chunk);
  return convert_result(result);
}

/* ========================================================================
 * Utility Functions
 * ======================================================================== */

const char *tapak_simdutf_error_to_string(tapak_simdutf_error_code_t error) {
  switch (error) {
  case TAPAK_SIMDUTF_ERROR_CODE_SUCCESS:
    return "SUCCESS";
  case TAPAK_SIMDUTF_ERROR_CODE_HEADER_BITS:
    return "HEADER_BITS";
  case TAPAK_SIMDUTF_ERROR_CODE_TOO_SHORT:
    return "TOO_SHORT";
  case TAPAK_SIMDUTF_ERROR_CODE_TOO_LONG:
    return "TOO_LONG";
  case TAPAK_SIMDUTF_ERROR_CODE_OVERLONG:
    return "OVERLONG";
  case TAPAK_SIMDUTF_ERROR_CODE_TOO_LARGE:
    return "TOO_LARGE";
  case TAPAK_SIMDUTF_ERROR_CODE_SURROGATE:
    return "SURROGATE";
  case TAPAK_SIMDUTF_ERROR_CODE_INVALID_BASE64_CHARACTER:
    return "INVALID_BASE64_CHARACTER";
  case TAPAK_SIMDUTF_ERROR_CODE_BASE64_INPUT_REMAINDER:
    return "BASE64_INPUT_REMAINDER";
  case TAPAK_SIMDUTF_ERROR_CODE_BASE64_EXTRA_BITS:
    return "BASE64_EXTRA_BITS";
  case TAPAK_SIMDUTF_ERROR_CODE_OUTPUT_BUFFER_TOO_SMALL:
    return "OUTPUT_BUFFER_TOO_SMALL";
  case TAPAK_SIMDUTF_ERROR_CODE_OTHER:
  default:
    return "OTHER";
  }
}
