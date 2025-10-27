ANY method route ppx functionality
  $ ../ppx.sh input.ml
  let webhook_handler_path =
    Tapak.Router.( / ) (Tapak.Router.s "api") (Tapak.Router.s "webhook")
  
  let resource_handler_path =
    Tapak.Router.( / ) (Tapak.Router.s "resources") Tapak.Router.int64
  
  let webhook_handler request = Tapak.Response.of_string' "Webhook received"
  [@@route ANY, "/api/webhook"]
  
  let resource_handler ~id request =
    Tapak.Response.of_string' (Format.sprintf "Resource: %Ld" id)
  [@@route ANY, "/resources/<int64:id>"]
  
  let webhook_handler_route =
    Tapak.Router.( @-> )
      (Tapak.Router.any
         (Tapak.Router.( / ) (Tapak.Router.s "api") (Tapak.Router.s "webhook")))
      webhook_handler
  
  let resource_handler_route =
    Tapak.Router.( @-> )
      (Tapak.Router.any
         (Tapak.Router.( / ) (Tapak.Router.s "resources") Tapak.Router.int64))
      (fun id request -> resource_handler ~id request)
