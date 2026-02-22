open! Base

let ( let* ) = Lwt.bind

type t = { status : status_t; headers : header_t list; body : string option }
and header_t = string * string

and status_t =
  | OkStatus
  | CreatedStatus
  | NotFoundStatus
  | BadRequestStatus
  | InternalServerErrorStatus

let status_code status =
  match status with
  | OkStatus -> 200
  | CreatedStatus -> 201
  | BadRequestStatus -> 400
  | NotFoundStatus -> 404
  | InternalServerErrorStatus -> 500

let status_code_text status =
  match status with
  | OkStatus -> "OK"
  | CreatedStatus -> "Created"
  | BadRequestStatus -> "Bad Request"
  | NotFoundStatus -> "Not Found"
  | InternalServerErrorStatus -> "Internal Server Error"

let response_with_body ?(status = OkStatus)
    ?(headers = [ ("Content-Type", "text/plain") ]) content =
  { status; headers; body = Some content }

let simple_response status = { status; headers = []; body = None }
let not_found () = simple_response NotFoundStatus

let file_response filename =
  let args = Args.parse_options Stdlib.Sys.argv in
  let dir =
    match args.directory with
    | Some dir -> dir
    | None -> Stdlib.Filename.current_dir_name
  in
  let path = Stdlib.Filename.concat dir filename in
  match Stdlib.Sys.file_exists path with
  | false -> not_found ()
  | true ->
      let content =
        In_channel.with_open_bin path (fun inch -> In_channel.input_all inch)
      in
      let headers = [ ("Content-Type", "application/octet-stream") ] in
      response_with_body ~headers content

let create_file_response filename content =
  match content with
  | None -> simple_response BadRequestStatus
  | Some content ->
      let args = Args.parse_options Stdlib.Sys.argv in
      let dir =
        match args.directory with
        | Some dir -> dir
        | None -> Stdlib.Filename.current_dir_name
      in
      let path = Stdlib.Filename.concat dir filename in
      Out_channel.with_open_bin path (fun outch ->
          Out_channel.output_string outch content)
      |> ignore;
      simple_response CreatedStatus

let compress compress response =
  match (response.body, compress) with
  | Some body, true ->
      {
        response with
        body = Some (Gzip.compress body);
        headers = ("Content-Encoding", "gzip") :: response.headers;
      }
  | _ -> response

let content_length response =
  match response.body with
  | None -> response
  | Some body ->
      {
        response with
        headers =
          ("Content-Length", String.length body |> Int.to_string)
          :: response.headers;
      }

let close (close : bool) (response : t) =
  if close then
    { response with headers = ("Connection", "close") :: response.headers }
  else response

let response_header_section headers =
  List.map headers ~f:(fun (key, value) ->
      Stdlib.Printf.sprintf "%s: %s" key value)
  |> String.concat ~sep:"\r\n"

let write (output : Lwt_io.output_channel) (response : t) =
  let* () =
    Lwt_io.fprintf output "HTTP/1.1 %d %s\r\n"
      (status_code response.status)
      (status_code_text response.status)
  in
  let* () = Lwt_io.write output (response_header_section response.headers) in
  let* () = Lwt_io.write output "\r\n" in
  match response.body with
  | None -> Lwt.return_unit
  | Some body -> Lwt_io.fprintf output "%s\r\n" body
