type t =
  { name : string
  ; filename : string option
  ; content_type : string
  ; body : Js.Blob.t
  }
