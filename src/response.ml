open! Base

type status_t =
  | OkStatus
  | NotFoundStatus
  | BadRequestStatus
  | InternalServerErrorStatus

let status_code status =
  match status with
  | OkStatus -> 200
  | BadRequestStatus -> 400
  | NotFoundStatus -> 404
  | InternalServerErrorStatus -> 500

let status_code_text status =
  match status with
  | OkStatus -> "OK"
  | BadRequestStatus -> "Bad Request"
  | NotFoundStatus -> "Not Found"
  | InternalServerErrorStatus -> "Internal Server Error"

let response_string_with_content status content =
  Stdlib.Printf.sprintf
    "HTTP/1.1 %d %s\r\nContent-Type: text/plain\r\nContent-Length: %d\r\n\r\n%s"
    (status_code status) (status_code_text status) (String.length content)
    content

let not_found () =
  Stdlib.Printf.sprintf "HTTP/1.1 %d %s\r\n\r\n"
    (status_code NotFoundStatus)
    (status_code_text NotFoundStatus)
