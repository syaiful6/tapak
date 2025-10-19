#include <brotli/decode.h>
#include <brotli/encode.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <string.h>

CAMLprim value tapak_brotli_decoder_create_instance(value unit) {
  CAMLparam1(unit);
  BrotliDecoderState *state = BrotliDecoderCreateInstance(NULL, NULL, NULL);
  if (state == NULL) {
    caml_failwith("Failed to create Brotli decoder instance");
  }
  CAMLreturn((value)state);
}

CAMLprim value tapak_brotli_decoder_destroy_instance(value state_val) {
  CAMLparam1(state_val);
  BrotliDecoderState *state = (BrotliDecoderState *)state_val;
  BrotliDecoderDestroyInstance(state);
  CAMLreturn(Val_unit);
}

CAMLprim value tapak_brotli_decompress_stream(value state_val, value input_val,
                                              value output_size_val) {
  CAMLparam3(state_val, input_val, output_size_val);
  CAMLlocal2(result, output_val);

  BrotliDecoderState *state = (BrotliDecoderState *)state_val;
  const uint8_t *input = (const uint8_t *)String_val(input_val);
  size_t input_size = caml_string_length(input_val);
  size_t available_in = input_size;
  const uint8_t *next_in = input;

  size_t output_buffer_size = Int_val(output_size_val);
  uint8_t *output_buffer = malloc(output_buffer_size);
  if (output_buffer == NULL) {
    caml_failwith("Failed to allocate output buffer");
  }

  size_t available_out = output_buffer_size;
  uint8_t *next_out = output_buffer;
  size_t total_out = 0;

  BrotliDecoderResult res = BrotliDecoderDecompressStream(
      state, &available_in, &next_in, &available_out, &next_out, &total_out);

  result = caml_alloc_tuple(3);

  int status;
  switch (res) {
  case BROTLI_DECODER_RESULT_ERROR:
    status = 0;
    break;
  case BROTLI_DECODER_RESULT_SUCCESS:
    status = 1;
    break;
  case BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT:
    status = 2;
    break;
  case BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT:
    status = 3;
    break;
  default:
    status = 0;
  }

  Store_field(result, 0, Val_int(status));
  Store_field(result, 1,
              Val_int(input_size - available_in)); /* bytes consumed */

  size_t output_size = output_buffer_size - available_out;
  output_val = caml_alloc_string(output_size);
  memcpy(Bytes_val(output_val), output_buffer, output_size);
  Store_field(result, 2, output_val);

  free(output_buffer);
  CAMLreturn(result);
}

CAMLprim value tapak_brotli_encoder_create_instance(value quality_val) {
  CAMLparam1(quality_val);
  BrotliEncoderState *state = BrotliEncoderCreateInstance(NULL, NULL, NULL);
  if (state == NULL) {
    caml_failwith("Failed to create Brotli encoder instance");
  }

  int quality = Int_val(quality_val);
  BrotliEncoderSetParameter(state, BROTLI_PARAM_QUALITY, quality);

  CAMLreturn((value)state);
}

CAMLprim value tapak_brotli_encoder_destroy_instance(value state_val) {
  CAMLparam1(state_val);
  BrotliEncoderState *state = (BrotliEncoderState *)state_val;
  BrotliEncoderDestroyInstance(state);
  CAMLreturn(Val_unit);
}

CAMLprim value tapak_brotli_compress_stream(value state_val, value input_val,
                                            value output_size_val,
                                            value op_val) {
  CAMLparam4(state_val, input_val, output_size_val, op_val);
  CAMLlocal2(result, output_val);

  BrotliEncoderState *state = (BrotliEncoderState *)state_val;
  const uint8_t *input = (const uint8_t *)String_val(input_val);
  size_t input_size = caml_string_length(input_val);
  size_t available_in = input_size;
  const uint8_t *next_in = input;

  /* op: 0 = process, 1 = flush, 2 = finish */
  BrotliEncoderOperation op;
  switch (Int_val(op_val)) {
  case 0:
    op = BROTLI_OPERATION_PROCESS;
    break;
  case 1:
    op = BROTLI_OPERATION_FLUSH;
    break;
  case 2:
    op = BROTLI_OPERATION_FINISH;
    break;
  default:
    op = BROTLI_OPERATION_PROCESS;
  }

  size_t output_buffer_size = Int_val(output_size_val);
  uint8_t *output_buffer = malloc(output_buffer_size);
  if (output_buffer == NULL) {
    caml_failwith("Failed to allocate output buffer");
  }

  size_t available_out = output_buffer_size;
  uint8_t *next_out = output_buffer;

  BROTLI_BOOL success = BrotliEncoderCompressStream(
      state, op, &available_in, &next_in, &available_out, &next_out, NULL);

  result = caml_alloc_tuple(4);

  Store_field(result, 0, Val_bool(success));
  Store_field(result, 1, Val_int(input_size - available_in));

  size_t output_size = output_buffer_size - available_out;
  output_val = caml_alloc_string(output_size);
  memcpy(Bytes_val(output_val), output_buffer, output_size);
  Store_field(result, 2, output_val);

  Store_field(result, 3, Val_bool(BrotliEncoderIsFinished(state)));

  free(output_buffer);
  CAMLreturn(result);
}
