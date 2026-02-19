open! Base

let response_string_with_content content =
  Stdlib.Printf.sprintf
    "HTTP/1.1 200 OK\r\n\
     Content-Type: text/plain\r\n\
     Content-Length: %d\r\n\
     \r\n\
     %s"
    (String.length content) content

let not_found () = "HTTP/1.1 404 Not Found\r\n\r\n"
