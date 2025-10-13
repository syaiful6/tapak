#ifndef TAPAK_SIMDUTF_CAPI_H
#define TAPAK_SIMDUTF_CAPI_H

#include <stddef.h>
#include <stdint.h>

#ifdef _WIN32
#ifdef TAPAK_SIMDUTF_EXPORTS
#define TAPAK_SIMDUTF_API __declspec(dllexport)
#else
#define TAPAK_SIMDUTF_API __declspec(dllimport)
#endif
#else // For GCC/Clang
#define TAPAK_SIMDUTF_API __attribute__((visibility("default")))
#endif

#ifdef __cplusplus
#define TAPAK_SIMDUTF_PLUS_PLUS_BEGIN_GUARD extern "C" {
#define TAPAK_SIMDUTF_PLUS_PLUS_END_GUARD }
#else
#include <stdbool.h>
#define TAPAK_SIMDUTF_PLUS_PLUS_BEGIN_GUARD
#define TAPAK_SIMDUTF_PLUS_PLUS_END_GUARD
#endif

TAPAK_SIMDUTF_PLUS_PLUS_BEGIN_GUARD

/* ========================================================================
 * Error Codes
 * ======================================================================== */

typedef enum tapak_simdutf_error_code_e {
  TAPAK_SIMDUTF_ERROR_CODE_SUCCESS = 0,
  TAPAK_SIMDUTF_ERROR_CODE_HEADER_BITS,
  TAPAK_SIMDUTF_ERROR_CODE_TOO_SHORT,
  TAPAK_SIMDUTF_ERROR_CODE_TOO_LONG,
  TAPAK_SIMDUTF_ERROR_CODE_OVERLONG,
  TAPAK_SIMDUTF_ERROR_CODE_TOO_LARGE,
  TAPAK_SIMDUTF_ERROR_CODE_SURROGATE,
  TAPAK_SIMDUTF_ERROR_CODE_INVALID_BASE64_CHARACTER,
  TAPAK_SIMDUTF_ERROR_CODE_BASE64_INPUT_REMAINDER,
  TAPAK_SIMDUTF_ERROR_CODE_BASE64_EXTRA_BITS,
  TAPAK_SIMDUTF_ERROR_CODE_OUTPUT_BUFFER_TOO_SMALL,
  TAPAK_SIMDUTF_ERROR_CODE_OTHER
} tapak_simdutf_error_code_t;

typedef struct tapak_simdutf_result_s {
  tapak_simdutf_error_code_t error;
  size_t count; /* Position of error or number of bytes processed */
} tapak_simdutf_result_t;

/* ========================================================================
 * Base64 Options
 * ======================================================================== */

typedef enum tapak_simdutf_base64_options_e {
  TAPAK_SIMDUTF_BASE64_OPTIONS_DEFAULT = 0,
  TAPAK_SIMDUTF_BASE64_OPTIONS_URL = 1,
  TAPAK_SIMDUTF_BASE64_OPTIONS_DEFAULT_NO_PADDING = 2,
  TAPAK_SIMDUTF_BASE64_OPTIONS_URL_WITH_PADDING = 3,
  TAPAK_SIMDUTF_BASE64_OPTIONS_DEFAULT_ACCEPT_GARBAGE = 4,
  TAPAK_SIMDUTF_BASE64_OPTIONS_URL_ACCEPT_GARBAGE = 5,
  TAPAK_SIMDUTF_BASE64_OPTIONS_DEFAULT_OR_URL = 8,
  TAPAK_SIMDUTF_BASE64_OPTIONS_DEFAULT_OR_URL_ACCEPT_GARBAGE = 12
} tapak_simdutf_base64_options_t;

typedef enum tapak_simdutf_last_chunk_options_e {
  TAPAK_SIMDUTF_BASE64_LCO_LOOSE = 0,
  TAPAK_SIMDUTF_BASE64_LCO_STRICT = 1,
  TAPAK_SIMDUTF_BASE64_LCO_STOP_BEFORE_PARTIAL = 2,
  TAPAK_SIMDUTF_BASE64_LCO_ONLY_FULL_CHUNKS = 3
} tapak_simdutf_last_chunk_options_t;

/* ========================================================================
 * UTF-8 Validation Functions
 * ======================================================================== */

/**
 * Validate a UTF-8 string.
 *
 * @param buf Pointer to the UTF-8 string to validate
 * @param len Length of the string in bytes
 * @return true if the string is valid UTF-8, false otherwise
 */
TAPAK_SIMDUTF_API bool tapak_simdutf_validate_utf8(const char *buf, size_t len);

/**
 * Validate a UTF-8 string and return detailed error information.
 *
 * @param buf Pointer to the UTF-8 string to validate
 * @param len Length of the string in bytes
 * @return Result structure with error code and position
 */
TAPAK_SIMDUTF_API tapak_simdutf_result_t
tapak_simdutf_validate_utf8_with_errors(const char *buf, size_t len);

/* ========================================================================
 * ASCII Validation Functions
 * ======================================================================== */

/**
 * Validate an ASCII string.
 *
 * @param buf Pointer to the ASCII string to validate
 * @param len Length of the string in bytes
 * @return true if the string is valid ASCII, false otherwise
 */
TAPAK_SIMDUTF_API bool tapak_simdutf_validate_ascii(const char *buf,
                                                    size_t len);

/**
 * Validate an ASCII string and return detailed error information.
 *
 * @param buf Pointer to the ASCII string to validate
 * @param len Length of the string in bytes
 * @return Result structure with error code and position
 */
TAPAK_SIMDUTF_API tapak_simdutf_result_t
tapak_simdutf_validate_ascii_with_errors(const char *buf, size_t len);

/* ========================================================================
 * UTF-16 Validation Functions
 * ======================================================================== */

/**
 * Validate a UTF-16 string.
 *
 * @param buf Pointer to the UTF-16 string to validate
 * @param len Length of the string in char16_t units
 * @return true if the string is valid UTF-16, false otherwise
 */
TAPAK_SIMDUTF_API bool tapak_simdutf_validate_utf16(const uint16_t *buf,
                                                    size_t len);

/**
 * Validate a UTF-16LE string.
 *
 * @param buf Pointer to the UTF-16LE string to validate
 * @param len Length of the string in char16_t units
 * @return true if the string is valid UTF-16LE, false otherwise
 */
TAPAK_SIMDUTF_API bool tapak_simdutf_validate_utf16le(const uint16_t *buf,
                                                      size_t len);

/**
 * Validate a UTF-16BE string.
 *
 * @param buf Pointer to the UTF-16BE string to validate
 * @param len Length of the string in char16_t units
 * @return true if the string is valid UTF-16BE, false otherwise
 */
TAPAK_SIMDUTF_API bool tapak_simdutf_validate_utf16be(const uint16_t *buf,
                                                      size_t len);

/**
 * Validate a UTF-16 string and return detailed error information.
 *
 * @param buf Pointer to the UTF-16 string to validate
 * @param len Length of the string in char16_t units
 * @return Result structure with error code and position
 */
TAPAK_SIMDUTF_API tapak_simdutf_result_t
tapak_simdutf_validate_utf16_with_errors(const uint16_t *buf, size_t len);

/**
 * Validate a UTF-16LE string and return detailed error information.
 *
 * @param buf Pointer to the UTF-16LE string to validate
 * @param len Length of the string in char16_t units
 * @return Result structure with error code and position
 */
TAPAK_SIMDUTF_API tapak_simdutf_result_t
tapak_simdutf_validate_utf16le_with_errors(const uint16_t *buf, size_t len);

/**
 * Validate a UTF-16BE string and return detailed error information.
 *
 * @param buf Pointer to the UTF-16BE string to validate
 * @param len Length of the string in char16_t units
 * @return Result structure with error code and position
 */
TAPAK_SIMDUTF_API tapak_simdutf_result_t
tapak_simdutf_validate_utf16be_with_errors(const uint16_t *buf, size_t len);

/* ========================================================================
 * UTF-32 Validation Functions
 * ======================================================================== */

/**
 * Validate a UTF-32 string.
 *
 * @param buf Pointer to the UTF-32 string to validate
 * @param len Length of the string in char32_t units
 * @return true if the string is valid UTF-32, false otherwise
 */
TAPAK_SIMDUTF_API bool tapak_simdutf_validate_utf32(const uint32_t *buf,
                                                    size_t len);

/**
 * Validate a UTF-32 string and return detailed error information.
 *
 * @param buf Pointer to the UTF-32 string to validate
 * @param len Length of the string in char32_t units
 * @return Result structure with error code and position
 */
TAPAK_SIMDUTF_API tapak_simdutf_result_t
tapak_simdutf_validate_utf32_with_errors(const uint32_t *buf, size_t len);

/* ========================================================================
 * Base64 Encoding Functions
 * ======================================================================== */

/**
 * Calculate the required output buffer size for base64 encoding.
 *
 * @param length Length of the binary input in bytes
 * @param options Base64 encoding options
 * @return Number of bytes required for the base64 output
 */
TAPAK_SIMDUTF_API size_t tapak_simdutf_base64_length_from_binary(
    size_t length, tapak_simdutf_base64_options_t options);

/**
 * Encode binary data to base64.
 *
 * @param input Pointer to the binary input data
 * @param length Length of the input in bytes
 * @param output Pointer to the output buffer (must be at least
 *               tapak_simdutf_base64_length_from_binary(length, options) bytes)
 * @param options Base64 encoding options
 * @return Number of bytes written to the output buffer
 */
TAPAK_SIMDUTF_API size_t
tapak_simdutf_binary_to_base64(const char *input, size_t length, char *output,
                               tapak_simdutf_base64_options_t options);

/* ========================================================================
 * Base64 Decoding Functions
 * ======================================================================== */

/**
 * Calculate the maximum required output buffer size for base64 decoding.
 *
 * @param input Pointer to the base64 input string
 * @param length Length of the input in bytes
 * @return Maximum number of bytes that could result from decoding
 */
TAPAK_SIMDUTF_API size_t tapak_simdutf_maximal_binary_length_from_base64(
    const char *input, size_t length);

/**
 * Decode base64 string to binary data.
 *
 * Follows the WHATWG forgiving-base64 format, which ignores ASCII spaces.
 * Input may be padded or unpadded.
 *
 * @param input Pointer to the base64 input string
 * @param length Length of the input in bytes
 * @param output Pointer to the output buffer (must be at least
 *               tapak_simdutf_maximal_binary_length_from_base64(input, length)
 * bytes)
 * @param options Base64 decoding options
 * @param last_chunk_options Last chunk handling options
 * @return Result structure with error code and number of bytes written
 */
TAPAK_SIMDUTF_API tapak_simdutf_result_t tapak_simdutf_base64_to_binary(
    const char *input, size_t length, char *output,
    tapak_simdutf_base64_options_t options,
    tapak_simdutf_last_chunk_options_t last_chunk_options);

/**
 * Calculate the maximum required output buffer size for base64 decoding (UTF-16
 * input).
 *
 * @param input Pointer to the base64 input string in UTF-16
 * @param length Length of the input in char16_t units
 * @return Maximum number of bytes that could result from decoding
 */
TAPAK_SIMDUTF_API size_t tapak_simdutf_maximal_binary_length_from_base64_u16(
    const uint16_t *input, size_t length);

/**
 * Decode base64 string to binary data (UTF-16 input).
 *
 * @param input Pointer to the base64 input string in UTF-16
 * @param length Length of the input in char16_t units
 * @param output Pointer to the output buffer
 * @param options Base64 decoding options
 * @param last_chunk_options Last chunk handling options
 * @return Result structure with error code and number of bytes written
 */
TAPAK_SIMDUTF_API tapak_simdutf_result_t tapak_simdutf_base64_to_binary_u16(
    const uint16_t *input, size_t length, char *output,
    tapak_simdutf_base64_options_t options,
    tapak_simdutf_last_chunk_options_t last_chunk_options);

/* ========================================================================
 * Utility Functions
 * ======================================================================== */

/**
 * Get a string description of an error code.
 *
 * @param error The error code
 * @return String description of the error (valid for the lifetime of the
 * program)
 */
TAPAK_SIMDUTF_API const char *
tapak_simdutf_error_to_string(tapak_simdutf_error_code_t error);

TAPAK_SIMDUTF_PLUS_PLUS_END_GUARD

#endif /* TAPAK_SIMDUTF_CAPI_H */
