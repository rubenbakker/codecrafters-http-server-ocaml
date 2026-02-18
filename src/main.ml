open Base

let ( let* ) = Lwt.bind

let handle_client (input, output) =
  let* request = Request.read input in

  Stdlib.print_endline
    (List.length (String.split ~on:'/' request.path) |> Int.to_string);
  let response =
    match String.split ~on:'/' request.path with
    | [] -> "HTTP/1.1 200 OK\r\n\r\n"
    | [ ""; "echo"; content ] ->
        Stdlib.Printf.sprintf
          "HTTP/1.1 200 OK\r\n\
           Content-Type: text/plain\r\n\
           Content-Length: %d\r\n\
           \r\n\
           %s"
          (String.length content) content
    | x -> "HTTP/1.1 404 Not Found\r\n\r\n"
  in

  let* () = Lwt_io.write_line output response in
  Lwt_io.printlf "Sent response" |> ignore;
  Lwt_io.flush output |> ignore;
  Lwt_io.close output

let rec accept_connections server_socket =
  let* client_socket, _addr = Lwt_unix.accept server_socket in
  let input = Lwt_io.of_fd ~mode:Lwt_io.input client_socket in
  let output = Lwt_io.of_fd ~mode:Lwt_io.output client_socket in
  Lwt.async (fun () -> handle_client (input, output));
  accept_connections server_socket

let start_server port =
  let sockaddr = Unix.(ADDR_INET (inet_addr_of_string "127.0.0.1", 4221)) in
  let server_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt server_socket Unix.SO_REUSEADDR true;
  let* () = Lwt_unix.bind server_socket sockaddr in
  Lwt_unix.listen server_socket 10;
  let* () = Lwt_io.printlf "Server started on port %d" port in
  accept_connections server_socket

let () = Lwt_main.run (start_server 4221)
