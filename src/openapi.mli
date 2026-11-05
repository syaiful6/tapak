module Spec = Openapi_spec

val generate :
   ?title:string
  -> ?version:string
  -> ?description:string
  -> ?base_path:string
  -> Router.route list
  -> Spec.t

val generate_string :
   ?title:string
  -> ?version:string
  -> ?description:string
  -> ?base_path:string
  -> Router.route list
  -> string

val generate_router2 :
   ?title:string
  -> ?version:string
  -> ?description:string
  -> ?base_path:string
  -> Router2.route list
  -> Spec.t

val generate_router2_string :
   ?title:string
  -> ?version:string
  -> ?description:string
  -> ?base_path:string
  -> Router2.route list
  -> string
