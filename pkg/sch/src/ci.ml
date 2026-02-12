(* Convert codes to upper case and compare them. This is a port of assembly code
   from the page:

   http://www.azillionmonkeys.com/qed/asmexample.html *)
let[@inline always] char_code_equal_ci x y =
  let codes = (x lsl 8) lor y in
  let b = 0x8080 lor codes in
  let c = b - 0x6161 in
  let d = lnot (b - 0x7b7b) in
  let e = c land d land (lnot codes land 0x8080) in
  let upper = codes - (e lsr 2) in
  upper lsr 8 = upper land 0xff

let equal x y =
  let len = String.length x in
  len = String.length y
  &&
  let equal_so_far = ref true in
  let i = ref 0 in
  while !equal_so_far && !i < len do
    let c1 = Char.code (String.unsafe_get x !i) in
    let c2 = Char.code (String.unsafe_get y !i) in
    equal_so_far := char_code_equal_ci c1 c2;
    incr i
  done;
  !equal_so_far
