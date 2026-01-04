val generate :
   ?title:string
  -> ?version:string
  -> ?description:string
  -> ?base_path:string
  -> Router.route list
  -> Yojson.Safe.t
