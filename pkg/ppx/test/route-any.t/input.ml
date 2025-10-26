let webhook_handler request =
  Tapak.Response.of_string' "Webhook received"
[@@route ANY, "/api/webhook"]

let resource_handler ~id request =
  Tapak.Response.of_string' (Format.sprintf "Resource: %Ld" id)
[@@route ANY, "/resources/<int64:id>"]
