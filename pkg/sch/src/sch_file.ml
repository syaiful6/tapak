type t =
  { name : string
  ; filename : string option
  ; content_type : string
  ; body : Bytesrw.Bytes.Reader.t
  }
