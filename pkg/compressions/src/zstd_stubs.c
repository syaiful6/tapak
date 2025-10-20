#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <string.h>
#include <zstd.h>

CAMLprim value tapak_zstd_create_dctx(value unit) {
  CAMLparam1(unit);
  ZSTD_DCtx *dctx = ZSTD_createDCtx();
  if (dctx == NULL) {
    caml_failwith("Failed to create Zstd decompression context");
  }
  CAMLreturn((value)dctx);
}

CAMLprim value tapak_zstd_free_dctx(value dctx_val) {
  CAMLparam1(dctx_val);
  ZSTD_DCtx *dctx = (ZSTD_DCtx *)dctx_val;
  ZSTD_freeDCtx(dctx);
  CAMLreturn(Val_unit);
}

CAMLprim value tapak_zstd_decompress_stream(value dctx_val, value input_val,
                                            value output_size_val) {
  CAMLparam3(dctx_val, input_val, output_size_val);
  CAMLlocal2(result, output_val);

  ZSTD_DCtx *dctx = (ZSTD_DCtx *)dctx_val;

  ZSTD_inBuffer input;
  input.src = String_val(input_val);
  input.size = caml_string_length(input_val);
  input.pos = 0;

  size_t output_buffer_size = Int_val(output_size_val);
  char *output_buffer = malloc(output_buffer_size);
  if (output_buffer == NULL) {
    caml_failwith("Failed to allocate output buffer");
  }

  ZSTD_outBuffer output;
  output.dst = output_buffer;
  output.size = output_buffer_size;
  output.pos = 0;

  size_t ret = ZSTD_decompressStream(dctx, &output, &input);

  result = caml_alloc_tuple(3);

  /* Status: 0 = error, 1 = finished, positive = hint for next input size */
  if (ZSTD_isError(ret)) {
    Store_field(result, 0, Val_int(0));
  } else if (ret == 0) {
    Store_field(result, 0, Val_int(1));
  } else {
    Store_field(result, 0, Val_int(2));
  }

  Store_field(result, 1, Val_int(input.pos)); /* bytes consumed */

  /* Create output string with decompressed data */
  output_val = caml_alloc_string(output.pos);
  memcpy(Bytes_val(output_val), output_buffer, output.pos);
  Store_field(result, 2, output_val);

  free(output_buffer);
  CAMLreturn(result);
}

/* Compression context management */
CAMLprim value tapak_zstd_create_cctx(value unit) {
  CAMLparam1(unit);
  ZSTD_CCtx *cctx = ZSTD_createCCtx();
  if (cctx == NULL) {
    caml_failwith("Failed to create Zstd compression context");
  }
  CAMLreturn((value)cctx);
}

CAMLprim value tapak_zstd_free_cctx(value cctx_val) {
  CAMLparam1(cctx_val);
  ZSTD_CCtx *cctx = (ZSTD_CCtx *)cctx_val;
  ZSTD_freeCCtx(cctx);
  CAMLreturn(Val_unit);
}

/* Set compression level */
CAMLprim value tapak_zstd_set_compression_level(value cctx_val,
                                                value level_val) {
  CAMLparam2(cctx_val, level_val);
  ZSTD_CCtx *cctx = (ZSTD_CCtx *)cctx_val;
  int level = Int_val(level_val);

  size_t ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_compressionLevel, level);
  if (ZSTD_isError(ret)) {
    caml_failwith("Failed to set compression level");
  }

  /* See https://issues.chromium.org/issues/41493659
   * For memory usage reasons, Chromium limits the window size to 8MB
   * See https://datatracker.ietf.org/doc/html/rfc8878#name-window-descriptor
   * For improved interoperability, it's recommended for decoders to support values
   * of Window_Size up to 8 MB and for encoders not to generate frames requiring a
   * Window_Size larger than 8 MB.
   *
   * Level 17 in zstd (as of v1.5.6) is the first level with a window size of 8 MB (2^23)
   * Set the parameter for all levels >= 17. This will either have no effect (but reduce
   * the risk of future changes in zstd) or limit the window log to 8MB.
   */
  if (level >= 17) {
    ret = ZSTD_CCtx_setParameter(cctx, ZSTD_c_windowLog, 23);
    if (ZSTD_isError(ret)) {
      caml_failwith("Failed to set window log parameter");
    }
  }

  CAMLreturn(Val_unit);
}

/* Streaming compression */
CAMLprim value tapak_zstd_compress_stream(value cctx_val, value input_val,
                                          value output_size_val, value end_val) {
  CAMLparam4(cctx_val, input_val, output_size_val, end_val);
  CAMLlocal2(result, output_val);

  ZSTD_CCtx *cctx = (ZSTD_CCtx *)cctx_val;
  int end_directive = Bool_val(end_val);

  ZSTD_inBuffer input;
  input.src = String_val(input_val);
  input.size = caml_string_length(input_val);
  input.pos = 0;

  size_t output_buffer_size = Int_val(output_size_val);
  char *output_buffer = malloc(output_buffer_size);
  if (output_buffer == NULL) {
    caml_failwith("Failed to allocate output buffer");
  }

  ZSTD_outBuffer output;
  output.dst = output_buffer;
  output.size = output_buffer_size;
  output.pos = 0;

  size_t ret;
  if (end_directive) {
    ret = ZSTD_endStream(cctx, &output);
  } else {
    ret = ZSTD_compressStream2(cctx, &output, &input, ZSTD_e_continue);
  }

  /* Create result tuple: (status, bytes_consumed, output_string, remaining) */
  result = caml_alloc_tuple(4);

  if (ZSTD_isError(ret)) {
    Store_field(result, 0, Val_int(0)); /* error */
  } else {
    Store_field(result, 0, Val_int(1)); /* success */
  }

  Store_field(result, 1, Val_int(input.pos)); /* bytes consumed */

  output_val = caml_alloc_string(output.pos);
  memcpy(Bytes_val(output_val), output_buffer, output.pos);
  Store_field(result, 2, output_val);

  Store_field(result, 3, Val_int(ret)); /* remaining bytes hint */

  free(output_buffer);
  CAMLreturn(result);
}
