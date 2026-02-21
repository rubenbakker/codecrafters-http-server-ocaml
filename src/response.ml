open! Base

type status_t =
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

let response_header_section headers =
  List.map headers ~f:(fun (key, value) ->
      Stdlib.Printf.sprintf "%s: %s" key value)
  |> String.concat ~sep:"\r\n"

let compress_string content = content

let response_string_with_content ?(status = OkStatus)
    ?(headers = [ ("Content-Type", "text/plain") ]) ?(compress = false) content
    =
  let content, headers =
    match compress with
    | false -> (content, headers)
    | true -> (compress_string content, headers)
  in
  Stdlib.Printf.sprintf "HTTP/1.1 %d %s\r\n%s\r\nContent-Length: %d\r\n\r\n%s"
    (status_code status) (status_code_text status)
    (response_header_section headers)
    (String.length content) content

let simple_response status =
  Stdlib.Printf.sprintf "HTTP/1.1 %d %s\r\n\r\n" (status_code status)
    (status_code_text status)

let not_found () =
  Stdlib.Printf.sprintf "HTTP/1.1 %d %s\r\n\r\n"
    (status_code NotFoundStatus)
    (status_code_text NotFoundStatus)

let file_response ~compress filename =
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
      response_string_with_content ~headers ~compress content

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
