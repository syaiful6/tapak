open Tapak
module Log = (val Logs.src_log Logs.default : Logs.LOG)

module Chat_room = struct
  type t =
    { clients : (int, Sse.Event.t option -> unit) Hashtbl.t
    ; mutable next_user_id : int
    }

  let create () = { clients = Hashtbl.create 10; next_user_id = 1 }

  let add_client t writer =
    let user_id = t.next_user_id in
    t.next_user_id <- t.next_user_id + 1;
    Hashtbl.add t.clients user_id writer;
    Log.info (fun m -> m "User %d joined the chat" user_id);
    user_id

  let remove_client t user_id =
    Hashtbl.remove t.clients user_id;
    Log.info (fun m -> m "User %d left the chat" user_id)

  let broadcast t ~sender_id message_text =
    let json =
      `Assoc
        [ "user", `Int sender_id
        ; "text", `String message_text
        ; "timestamp", `Float (Unix.gettimeofday ())
        ]
    in
    let event =
      Sse.Event.
        { id = None
        ; data = Some (`Json json)
        ; event = Some "message"
        ; comment = None
        ; retry = None
        }
    in
    Hashtbl.iter
      (fun user_id writer ->
         if user_id <> sender_id
         then
           try writer (Some event) with
           | _ ->
             Log.warn (fun m -> m "Failed to send message to user %d" user_id))
      t.clients
end

let html_page =
  {|<!DOCTYPE html>
<html>
<head>
  <meta charset="UTF-8">
  <title>SSE Chat</title>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
      max-width: 800px;
      margin: 40px auto;
      padding: 0 20px;
      background: #f5f5f5;
    }
    .container {
      background: white;
      border-radius: 8px;
      padding: 20px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    h1 {
      margin-top: 0;
      color: #333;
    }
    #status {
      padding: 10px;
      margin-bottom: 20px;
      border-radius: 4px;
      font-weight: bold;
    }
    #status.connected {
      background: #d4edda;
      color: #155724;
    }
    #status.disconnected {
      background: #f8d7da;
      color: #721c24;
    }
    #messages {
      height: 400px;
      overflow-y: auto;
      border: 1px solid #ddd;
      border-radius: 4px;
      padding: 15px;
      margin-bottom: 20px;
      background: #fafafa;
    }
    .message {
      margin-bottom: 10px;
      padding: 8px 12px;
      border-radius: 4px;
    }
    .message.own {
      background: #007bff;
      color: white;
      margin-left: 20%;
    }
    .message.other {
      background: #e9ecef;
      color: #333;
      margin-right: 20%;
    }
    .message .user {
      font-weight: bold;
      margin-right: 8px;
    }
    .message .time {
      font-size: 0.8em;
      opacity: 0.7;
      margin-left: 8px;
    }
    #input-container {
      display: flex;
      gap: 10px;
    }
    #message-input {
      flex: 1;
      padding: 10px;
      border: 1px solid #ddd;
      border-radius: 4px;
      font-size: 14px;
    }
    #send-button {
      padding: 10px 20px;
      background: #007bff;
      color: white;
      border: none;
      border-radius: 4px;
      cursor: pointer;
      font-size: 14px;
      font-weight: bold;
    }
    #send-button:hover {
      background: #0056b3;
    }
    #send-button:disabled {
      background: #ccc;
      cursor: not-allowed;
    }
  </style>
</head>
<body>
  <div class="container">
    <h1>SSE Chat Example</h1>
    <div id="status" class="disconnected">Connecting...</div>
    <div id="messages"></div>
    <div id="input-container">
      <input type="text" id="message-input" placeholder="Type a message..." disabled>
      <button id="send-button" disabled>Send</button>
    </div>
  </div>

  <script>
    let userId = null;
    let eventSource = null;
    const messagesDiv = document.getElementById('messages');
    const statusDiv = document.getElementById('status');
    const messageInput = document.getElementById('message-input');
    const sendButton = document.getElementById('send-button');

    function addMessage(msg, isOwn) {
      const messageDiv = document.createElement('div');
      messageDiv.className = 'message ' + (isOwn ? 'own' : 'other');

      const time = new Date(msg.timestamp * 1000).toLocaleTimeString();
      messageDiv.innerHTML = `
        <span class="user">User ${msg.user}</span>
        <span class="text">${escapeHtml(msg.text)}</span>
        <span class="time">${time}</span>
      `;

      messagesDiv.appendChild(messageDiv);
      messagesDiv.scrollTop = messagesDiv.scrollHeight;
    }

    function escapeHtml(text) {
      const div = document.createElement('div');
      div.textContent = text;
      return div.innerHTML;
    }

    function connect() {
      eventSource = new EventSource('/chat');

      eventSource.addEventListener('open', () => {
        statusDiv.textContent = 'Connected';
        statusDiv.className = 'status connected';
        messageInput.disabled = false;
        sendButton.disabled = false;
        console.log('Connected to chat');
      });

      eventSource.addEventListener('user-id', (e) => {
        userId = parseInt(e.data);
        statusDiv.textContent = `Connected as User ${userId}`;
        console.log('Received user ID:', userId);
      });

      eventSource.addEventListener('message', (e) => {
        const msg = JSON.parse(e.data);
        addMessage(msg, msg.user === userId);
      });

      eventSource.addEventListener('error', (e) => {
        statusDiv.textContent = 'Disconnected';
        statusDiv.className = 'status disconnected';
        messageInput.disabled = true;
        sendButton.disabled = true;
        console.error('Connection error:', e);

        // Attempt to reconnect
        setTimeout(() => {
          if (eventSource.readyState === EventSource.CLOSED) {
            console.log('Attempting to reconnect...');
            connect();
          }
        }, 3000);
      });
    }

    async function sendMessage() {
      const text = messageInput.value.trim();
      if (!text || userId === null) return;

      try {
        const response = await fetch(`/chat/${userId}`, {
          method: 'POST',
          headers: {
            'Content-Type': 'text/plain'
          },
          body: text
        });

        if (response.ok) {
          // Add own message to display
          addMessage({
            user: userId,
            text: text,
            timestamp: Date.now() / 1000
          }, true);
          messageInput.value = '';
        } else {
          console.error('Failed to send message:', response.status);
        }
      } catch (error) {
        console.error('Error sending message:', error);
      }
    }

    messageInput.addEventListener('keypress', (e) => {
      if (e.key === 'Enter') {
        sendMessage();
      }
    });

    sendButton.addEventListener('click', sendMessage);

    // Start connection
    connect();
  </script>
</body>
</html>|}

let home_handler _req = Response.of_html ~status:`OK html_page

let chat_stream_handler ~clock room req =
  let sw =
    match (Request.info req).sw with
    | Some sw -> sw
    | None -> failwith "No switch available in request"
  in
  let piaf_stream, piaf_writer = Piaf.Stream.create 4 in
  let user_id = Chat_room.add_client room piaf_writer in
  let user_id_event =
    Sse.Event.
      { id = None
      ; data = Some (`Text (string_of_int user_id))
      ; event = Some "user-id"
      ; comment = None
      ; retry = None
      }
  in
  piaf_writer (Some user_id_event);
  Eio.Switch.on_release sw (fun () -> Chat_room.remove_client room user_id);
  let kept_alive = Sse.keep_alive ~sw ~clock piaf_stream in
  Sse.stream kept_alive

let post_message_handler room user_id req =
  let body_str =
    Request.body req |> Body.to_string |> Result.get_ok |> String.trim
  in
  if String.length body_str = 0
  then Response.of_string' ~status:`Bad_request "Message cannot be empty"
  else if String.length body_str > 500
  then
    Response.of_string' ~status:`Bad_request "Message too long (max 500 bytes)"
  else (
    Chat_room.broadcast room ~sender_id:user_id body_str;
    Response.of_string' ~status:`OK "Message sent")

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  Eio_main.run @@ fun env ->
  let room = Chat_room.create () in
  let clock = Eio.Stdenv.clock env in
  let open Router in
  let app =
    App.(
      routes
        [ get (s "") @-> home_handler
        ; get (s "chat") @-> chat_stream_handler ~clock room
        ; ( post (s "chat" / int) @-> fun user_id req ->
            post_message_handler room user_id req )
        ]
        ())
  in
  let port = 8080 in
  let address = `Tcp (Eio.Net.Ipaddr.V4.any, port) in
  let config = Piaf.Server.Config.create address in
  Log.info (fun m -> m "Starting SSE chat server on http://localhost:%d" port);
  Log.info (fun m ->
    m "Open http://localhost:%d in multiple browser tabs to chat" port);
  ignore (Server.run_with ~config ~env app)
