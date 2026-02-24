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
