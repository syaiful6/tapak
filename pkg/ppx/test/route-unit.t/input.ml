let home () = Tapak.Response.of_string' "Home" [@@route GET, "/"]
let about () = Tapak.Response.of_string' "About" [@@route GET, "/about"]
