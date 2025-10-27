Django-style typed parameters
  $ ../ppx.sh input.ml
  let get_user_path =
    Tapak.Router.( / ) (Tapak.Router.s "users") Tapak.Router.int64
  
  let get_post_path =
    Tapak.Router.( / ) (Tapak.Router.s "posts") Tapak.Router.slug
  
  let toggle_path = Tapak.Router.( / ) (Tapak.Router.s "toggle") Tapak.Router.bool
  let get_article_path = Tapak.Router.( / ) (Tapak.Router.s "articles") (uuid ())
  
  let get_user ~id request =
    Tapak.Response.of_string' (Format.sprintf "User ID: %Ld" id)
  [@@route GET, "/users/<int64:id>"]
  
  let get_post ~slug request =
    Tapak.Response.of_string' (Format.sprintf "Post: %s" slug)
  [@@route GET, "/posts/<slug:slug>"]
  
  let toggle ~enabled request =
    Tapak.Response.of_string' (Format.sprintf "Enabled: %b" enabled)
  [@@route GET, "/toggle/<bool:enabled>"]
  
  let get_article ~id request =
    Tapak.Response.of_string' (Format.sprintf "Article: %s" id)
  [@@route GET, "/articles/<uuid:id>"]
  
  let get_user_route =
    Tapak.Router.( @-> )
      (Tapak.Router.get
         (Tapak.Router.( / ) (Tapak.Router.s "users") Tapak.Router.int64))
      (fun id request -> get_user ~id request)
  
  let get_post_route =
    Tapak.Router.( @-> )
      (Tapak.Router.get
         (Tapak.Router.( / ) (Tapak.Router.s "posts") Tapak.Router.slug))
      (fun slug request -> get_post ~slug request)
  
  let toggle_route =
    Tapak.Router.( @-> )
      (Tapak.Router.get
         (Tapak.Router.( / ) (Tapak.Router.s "toggle") Tapak.Router.bool))
      (fun enabled request -> toggle ~enabled request)
  
  let get_article_route =
    Tapak.Router.( @-> )
      (Tapak.Router.get
         (Tapak.Router.( / ) (Tapak.Router.s "articles") (uuid ())))
      (fun id request -> get_article ~id request)
