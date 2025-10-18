type bigaray =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let ascii_alpha_digits =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"

let () = Mirage_crypto_rng_unix.use_getentropy ()

let string ?allowed_chars n =
  let charset =
    if Option.is_none allowed_chars
    then ascii_alpha_digits
    else Option.get allowed_chars
  in
  let charset_len = String.length charset in
  let random_bytes = Mirage_crypto_rng.generate n in
  String.init n (fun i ->
    let byte = int_of_char random_bytes.[i] in
    charset.[byte mod charset_len])
