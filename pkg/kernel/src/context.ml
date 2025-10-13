type 'a field_metadata =
  { name : string option
  ; show : ('a -> string) option
  }

include Hmap.Make (struct
    type 'a t = 'a field_metadata
  end)
