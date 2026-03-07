#include <brotli/decode.h>
#include <brotli/encode.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <string.h>

#define Tapak_br_dec_val(v) (*((BrotliDecoderState **)Data_custom_val(v)))
#define Tapak_br_enc_val(v) (*((BrotliEncoderState **)Data_custom_val(v)))

static void tapak_finalize_brotli_decoder(value v) {
  BrotliDecoderState *state = Tapak_br_dec_val(v);
  if (state != NULL) {
    BrotliDecoderDestroyInstance(state);
  }
}

static struct custom_operations tapak_brotli_decoder_ops = {
    "tapak_brotli_decoder",    tapak_finalize_brotli_decoder,
    custom_compare_default,    custom_hash_default,
    custom_serialize_default,  custom_deserialize_default,
    custom_compare_ext_default};

static void tapak_finalize_brotli_encoder(value v) {
  BrotliEncoderState *state = Tapak_br_enc_val(v);
  if (state != NULL) {
    BrotliEncoderDestroyInstance(state);
  }
}

static struct custom_operations tapak_brotli_encoder_ops = {
    "tapak_brotli_encoder",    tapak_finalize_brotli_encoder,
    custom_compare_default,    custom_hash_default,
    custom_serialize_default,  custom_deserialize_default,
    custom_compare_ext_default};

enum tapak_bbuf_fields {
  tapak_bbuf_bytes = 0,
  tapak_bbuf_size,
  tapak_bbuf_pos
};

CAMLprim value tapak_brotli_decoder_create_instance(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(v);
  BrotliDecoderState *state = BrotliDecoderCreateInstance(NULL, NULL, NULL);
  if (state == NULL) {
    caml_failwith("Failed to create Brotli decoder instance");
  }
  v = caml_alloc_custom(&tapak_brotli_decoder_ops, sizeof(BrotliDecoderState *),
                        0, 1);
  Tapak_br_dec_val(v) = state;
  CAMLreturn(v);
}

CAMLprim value tapak_brotli_decoder_destroy_instance(value state_val) {
  CAMLparam1(state_val);
  BrotliDecoderState *state = Tapak_br_dec_val(state_val);
  if (state != NULL) {
    BrotliDecoderDestroyInstance(state);
    Tapak_br_dec_val(state_val) = NULL;
  }
  CAMLreturn(Val_unit);
}

CAMLprim value tapak_brotli_decompress_stream(value state, value src,
                                              value dst) {
  CAMLparam3(state, src, dst);
  BrotliDecoderState *decoder_state = Tapak_br_dec_val(state);
  /* input buffer */
  size_t src_pos = Long_val(Field(src, tapak_bbuf_pos));
  size_t src_size = Long_val(Field(src, tapak_bbuf_size));
  const uint8_t *next_in =
      (const uint8_t *)Bytes_val(Field(src, tapak_bbuf_bytes)) + src_pos;
  size_t available_in = src_size - src_pos;
  /* output buffer */
  size_t dst_pos = Long_val(Field(dst, tapak_bbuf_pos));
  size_t dst_size = Long_val(Field(dst, tapak_bbuf_size));
  uint8_t *next_out =
      (uint8_t *)Bytes_val(Field(dst, tapak_bbuf_bytes)) + dst_pos;
  size_t available_out = dst_size - dst_pos;

  BrotliDecoderResult res = BrotliDecoderDecompressStream(
      decoder_state, &available_in, &next_in, &available_out, &next_out, NULL);

  /* update positions */
  Store_field(src, tapak_bbuf_pos, Val_long(src_size - available_in));
  Store_field(dst, tapak_bbuf_pos, Val_long(dst_size - available_out));

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

  CAMLreturn(Val_int(status));
}

CAMLprim value tapak_brotli_encoder_create_instance(value quality_val) {
  CAMLparam1(quality_val);
  CAMLlocal1(v);
  BrotliEncoderState *state = BrotliEncoderCreateInstance(NULL, NULL, NULL);
  if (state == NULL) {
    caml_failwith("Failed to create Brotli encoder instance");
  }

  int quality = Int_val(quality_val);
  BrotliEncoderSetParameter(state, BROTLI_PARAM_QUALITY, quality);

  v = caml_alloc_custom(&tapak_brotli_encoder_ops, sizeof(BrotliEncoderState *),
                        0, 1);
  Tapak_br_enc_val(v) = state;
  CAMLreturn(v);
}

CAMLprim value tapak_brotli_encoder_destroy_instance(value state_val) {
  CAMLparam1(state_val);
  BrotliEncoderState *state = Tapak_br_enc_val(state_val);
  if (state != NULL) {
    BrotliEncoderDestroyInstance(state);
    Tapak_br_enc_val(state_val) = NULL;
  }
  CAMLreturn(Val_unit);
}

CAMLprim value tapak_brotli_compress_stream(value state_val, value src,
                                            value dst, value op_val) {
  CAMLparam4(state_val, src, dst, op_val);

  BrotliEncoderState *state = Tapak_br_enc_val(state_val);
  /* input buffer */
  size_t src_pos = Long_val(Field(src, tapak_bbuf_pos));
  size_t src_size = Long_val(Field(src, tapak_bbuf_size));
  const uint8_t *next_in =
      (const uint8_t *)Bytes_val(Field(src, tapak_bbuf_bytes)) + src_pos;
  size_t available_in = src_size - src_pos;
  /* output buffer */
  size_t dst_pos = Long_val(Field(dst, tapak_bbuf_pos));
  size_t dst_size = Long_val(Field(dst, tapak_bbuf_size));
  uint8_t *next_out =
      (uint8_t *)Bytes_val(Field(dst, tapak_bbuf_bytes)) + dst_pos;
  size_t available_out = dst_size - dst_pos;

  /* op: 0 = process, 1 = flush, 2 = finish */
  BrotliEncoderOperation op;
  switch (Int_val(op_val)) {
  case 1:  op = BROTLI_OPERATION_FLUSH;   break;
  case 2:  op = BROTLI_OPERATION_FINISH;  break;
  default: op = BROTLI_OPERATION_PROCESS; break;
  }

  BROTLI_BOOL success = BrotliEncoderCompressStream(
      state, op, &available_in, &next_in, &available_out, &next_out, NULL);

  Store_field(src, tapak_bbuf_pos, Val_long(src_size - available_in));
  Store_field(dst, tapak_bbuf_pos, Val_long(dst_size - available_out));

  /* 0 = error, 1 = ok, 2 = ok and finished */
  int status;
  if (!success) {
    status = 0;
  } else if (BrotliEncoderIsFinished(state)) {
    status = 2;
  } else {
    status = 1;
  }

  CAMLreturn(Val_int(status));
}
