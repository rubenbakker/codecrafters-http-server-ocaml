open Base

let ( let* ) = Lwt.bind

let rec handle_client (input, output) =
  let* request = Request.read input in
  let needs_to_close_connection = Request.needs_to_close_connection request in
  (match needs_to_close_connection with
    | true -> Lwt_io.eprintlf ">>> needs_to_close_connection"
    | false -> Lwt_io.eprintlf ">>> NO needs_to_close_connection")
  |> ignore;
  let compress = Request.gzip_accept_encoding request in
  let response =
    match (request.method_, request.path) with
    | Request.Get, [] -> Response.simple_response Response.OkStatus
    | Request.Get, [ "echo"; content ] ->
        Response.response_string_with_content ~compress content
    | Request.Get, [ "user-agent" ] -> (
        let user_agent = Request.header request "user-agent" in
        match user_agent with
        | Some user_agent ->
            Response.response_string_with_content ~compress user_agent
        | None -> Response.not_found ())
    | Request.Post, [ "files"; filename ] ->
        Response.create_file_response filename request.content
    | Request.Get, [ "files"; filename ] ->
        Response.file_response ~compress filename
    | _ -> Response.not_found ()
  in
  let* () = Lwt_io.write output response in
  match needs_to_close_connection with
  | false -> handle_client (input, output)
  | true ->
      Lwt_io.flush output |> ignore;
      Lwt_io.close output

let rec accept_connections server_socket =
  let* client_socket, _addr = Lwt_unix.accept server_socket in
  let input = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let output = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in
  Lwt.async (fun () -> handle_client (input, output));
  accept_connections server_socket

let start_server port =
  Stdlib.flush Stdlib.stdout;
  let sockaddr = Unix.(ADDR_INET (inet_addr_of_string "127.0.0.1", 4221)) in
  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  let* () = Lwt_unix.bind server_socket sockaddr in
  Lwt_unix.listen server_socket 10;
  let* () = Lwt_io.printlf "Server started on port %d" port in
  accept_connections server_socket

let () = Lwt_main.run (start_server 4221)
