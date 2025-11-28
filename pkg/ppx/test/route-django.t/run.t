Django-style typed parameters
  $ ../ppx.sh input.ml
  let get_user_path =
    Tapak.Router.( / ) (Tapak.Router.s "users")
      (Tapak.Router.p "id" Tapak.Router.int64)
  
  let get_post_path =
    Tapak.Router.( / ) (Tapak.Router.s "posts")
      (Tapak.Router.p "slug" Tapak.Router.slug)
  
  let toggle_path =
    Tapak.Router.( / ) (Tapak.Router.s "toggle")
      (Tapak.Router.p "enabled" Tapak.Router.bool)
  
  let get_article_path =
    Tapak.Router.( / ) (Tapak.Router.s "articles") (Tapak.Router.p "id" (uuid ()))
  
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
    Tapak.Router.get
      (Tapak.Router.( / ) (Tapak.Router.s "users")
         (Tapak.Router.p "id" Tapak.Router.int64))
    |> Tapak.Router.into (fun id request -> get_user ~id request)
  
  let get_post_route =
    Tapak.Router.get
      (Tapak.Router.( / ) (Tapak.Router.s "posts")
         (Tapak.Router.p "slug" Tapak.Router.slug))
    |> Tapak.Router.into (fun slug request -> get_post ~slug request)
  
  let toggle_route =
    Tapak.Router.get
      (Tapak.Router.( / ) (Tapak.Router.s "toggle")
         (Tapak.Router.p "enabled" Tapak.Router.bool))
    |> Tapak.Router.into (fun enabled request -> toggle ~enabled request)
  
  let get_article_route =
    Tapak.Router.get
      (Tapak.Router.( / )
         (Tapak.Router.s "articles")
         (Tapak.Router.p "id" (uuid ())))
    |> Tapak.Router.into (fun id request -> get_article ~id request)
