type t = Piaf.Body.t

type length =
  [ `Chunked
  | `Close_delimited
  | `Error of [ `Bad_gateway | `Bad_request | `Internal_server_error ]
  | `Fixed of Int64.t
  | `Unknown
  ]

val length : t -> length
val empty : t
val of_stream : ?length:length -> Bigstringaf.t Piaf.IOVec.t Piaf.Stream.t -> t
val of_string_stream : ?length:length -> string Piaf.Stream.t -> t
val of_string : string -> t
val of_bigstring : ?off:int -> ?len:int -> Bigstringaf.t -> t
val sendfile : ?length:length -> string -> (t, [> Piaf.Error.common ]) result
val to_string : t -> (string, [> Piaf.Error.t ]) result
val drain : t -> (unit, [> Piaf.Error.t ]) result
val is_closed : t -> bool
val closed : t -> (unit, [> Piaf.Error.t ]) result
val when_closed : f:((unit, [> Piaf.Error.t ]) result -> unit) -> t -> unit
val is_errored : t -> bool
val to_list : t -> Bigstringaf.t Piaf.IOVec.t list
val to_string_list : t -> string list

val fold :
   f:('a -> Bigstringaf.t Piaf.IOVec.t -> 'a)
  -> init:'a
  -> t
  -> ('a, [> Piaf.Error.t ]) result

val fold_string :
   f:('a -> string -> 'a)
  -> init:'a
  -> t
  -> ('a, [> Piaf.Error.t ]) result

val iter :
   f:(Bigstringaf.t Piaf.IOVec.t -> unit)
  -> t
  -> (unit, [> Piaf.Error.t ]) result

val iter_p :
   sw:Eio.Switch.t
  -> f:(Bigstringaf.t Piaf.IOVec.t -> unit)
  -> t
  -> (unit, [> Piaf.Error.t ]) result

val iter_string : f:(string -> unit) -> t -> (unit, [> Piaf.Error.t ]) result

val iter_string_p :
   sw:Eio.Switch.t
  -> f:(string -> unit)
  -> t
  -> (unit, [> Piaf.Error.t ]) result

val to_stream : t -> Bigstringaf.t Piaf.IOVec.t Piaf.Stream.t
val to_string_stream : t -> string Piaf.Stream.t
