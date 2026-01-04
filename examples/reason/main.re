let trusted_proxies = [
  Ipaddr.Prefix.of_string_exn("127.0.0.1/32"),
  Ipaddr.Prefix.of_string_exn("::1/128"),
];

[@route (GET, "/")]
let home = () =>
  Tapak.Response.of_string(~body="Welcome to Tapak with ReasonML!", `OK);

[@route (POST, "/users")]
let create_user = () => {
  let body = "User created successfully!";
  Tapak.Response.of_string(~body, `Created);
};

[@route (PUT, "/users/<int64:id>")]
let update_user = (~id) => {
  let body = Printf.sprintf("User %Ld updated!", id);
  Tapak.Response.of_string(~body, `OK);
};

[@route (DELETE, "/users/<int64:id>")]
let delete_user = (~id) => {
  let body = Printf.sprintf("User %Ld deleted!", id);
  Tapak.Response.of_string(~body, `OK);
};

[@route (GET, "/users/<int64:id>")]
let get_user = (~id) => {
  let edit_url = Tapak.Router.sprintf(edit_user_path, id);
  let delete_url = Tapak.Router.sprintf(delete_user_path, id);

  let body =
    Printf.sprintf(
      "User %Ld\nEdit: %s\nDelete: %s",
      id,
      edit_url,
      delete_url,
    );

  Tapak.Response.of_string(~body, `OK);
};

[@route (GET, "/users/<int64:id>/edit")]
let edit_user = (~id) => {
  let back_url = Tapak.Router.sprintf(get_user_path, id);

  let body = Printf.sprintf("Edit User %Ld\nBack to: %s", id, back_url);

  Tapak.Response.of_string(~body, `OK);
};

[@route (GET, "/users")]
let list_users = () => {
  let user_1_url = Tapak.Router.sprintf(get_user_path, 1L);
  let user_2_url = Tapak.Router.sprintf(get_user_path, 2L);
  let user_3_url = Tapak.Router.sprintf(get_user_path, 3L);

  let body =
    Printf.sprintf(
      "Users:\n- User 1: %s\n- User 2: %s\n- User 3: %s",
      user_1_url,
      user_2_url,
      user_3_url,
    );

  Tapak.Response.of_string(~body, `OK);
};

[@route (GET, "/posts/<slug:slug>")]
let get_post = (~slug) => {
  let body = Printf.sprintf("Post: %s", slug);
  Tapak.Response.of_string(~body, `OK);
};

[@route (GET, "/api/feature/<bool:enabled>")]
let toggle_feature = (~enabled) => {
  let status = enabled ? "enabled" : "disabled";
  let body = Printf.sprintf("Feature is %s", status);
  Tapak.Response.of_string(~body, `OK);
};

[@route (GET, "/search/<string:query>")]
let search = (~query) => {
  let body = Printf.sprintf("Searching for: %s", query);
  Tapak.Response.of_string(~body, `OK);
};

[@route (GET, "/files/**")]
let serve_file = (~splat) => {
  let file_path = String.concat("/", splat);
  let body = Printf.sprintf("Serving file: %s", file_path);
  Tapak.Response.of_string(~body, `OK);
};

[@route (ANY, "/webhook")]
let webhook_handler = request => {
  let method_str = Piaf.Method.to_string(Tapak.Request.meth(request));
  let body = Printf.sprintf("Webhook received via %s", method_str);
  Tapak.Response.of_string(~body, `OK);
};

[@route (ANY, "/api/resources/<int64:id>")]
let resource_handler = (~id) => {
  // TODO: the ppx can't handle request parameter yet
  // let method_str = Piaf.Method.to_string(Request.meth(request));
  let method_str = "GET";
  let body = Printf.sprintf("Resource %Ld accessed via %s", id, method_str);
  Tapak.Response.of_string(~body, `OK);
};

let not_found = _request =>
  Tapak.Response.of_string(~body="404 Not Found", `Not_found);

let setup_logging = (~threaded=false, ~style_renderer=?, level) => {
  if (threaded) {
    Logs_threaded.enable();
  } else {
    ();
  };
  Fmt_tty.setup_std_outputs(~style_renderer?, ());
  Logs.set_level(level);
  Logs.set_reporter(Logs_fmt.reporter());
};

let () = {
  setup_logging(~threaded=false, Some(Logs.Debug));
  Eio_main.run(env => {
    let now = () => Eio.Time.now(Eio.Stdenv.clock(env));
    let app =
      Tapak.(
        Router.routes(
          ~not_found,
          [
            home_route,
            list_users_route,
            get_user_route,
            create_user_route,
            edit_user_route,
            update_user_route,
            delete_user_route,
            get_post_route,
            toggle_feature_route,
            search_route,
            serve_file_route,
            webhook_handler_route,
            resource_handler_route,
          ],
        )
        |> use(
             (module Middleware.Request_logger),
             Middleware.Request_logger.args(~now, ~trusted_proxies, ()),
           )
      );
    let address = `Tcp((Eio.Net.Ipaddr.V4.loopback, 8080));
    let config = Piaf.Server.Config.create(address);
    ignore(Tapak.run_with(~config, ~env, app));
  });
};
