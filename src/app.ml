include Tapak_kernel.App

(** [routes routes] creates an app with the given routes. *)
let routes ?not_found routes = create ~handler:(Router.routes ?not_found routes)
