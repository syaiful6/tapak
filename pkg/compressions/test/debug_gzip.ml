(* Quick debug script *)
let () =
  let zstream = Zlib.deflate_init 6 true in
  let data = "test" in
  let buf = Bytes.create 4096 in
  let result_buf = Buffer.create 100 in

  let is_end, used_in, used_out =
    Zlib.deflate_string
      zstream
      data
      0
      (String.length data)
      buf
      0
      (Bytes.length buf)
      Zlib.Z_NO_FLUSH
  in
  if used_out > 0 then Buffer.add_subbytes result_buf buf 0 used_out;

  let is_end2, _, used_out2 =
    Zlib.deflate_string zstream "" 0 0 buf 0 (Bytes.length buf) Zlib.Z_FINISH
  in
  if used_out2 > 0 then Buffer.add_subbytes result_buf buf 0 used_out2;
  Zlib.deflate_end zstream;

  let compressed = Buffer.contents result_buf in
  Printf.printf "Compressed length: %d\n" (String.length compressed);
  Printf.printf "First 10 bytes (hex): ";
  for i = 0 to min 9 (String.length compressed - 1) do
    Printf.printf "%02x " (Char.code compressed.[i])
  done;
  Printf.printf "\n"
