open Base

type method_t = Get | Head | Post | Put | Patch | Delete

type t = {
  method_ : method_t;
  path : string list;
  headers : (string * string) list;
  content : string option;
}

exception RequestError of string

let ( let* ) = Lwt.bind

let convert_method method_string =
  match String.lowercase method_string with
  | "get" -> Get
  | "head" -> Head
  | "post" -> Post
  | "put" -> Put
  | "patch" -> Patch
  | "delete" -> Delete
  | _ ->
      raise
        (RequestError (Stdlib.Printf.sprintf "invalid method %s" method_string))

let content_length headers =
  List.map headers ~f:(fun (key, value) ->
      if String.(lowercase key = "content-length") then
        Some (Int.of_string value)
      else None)
  |> List.filter_opt |> List.hd

let parse_request_line line =
  match String.split line ~on:' ' with
  | [ method_; path; _ ] ->
      ( convert_method method_,
        String.split ~on:'/' path
        |> List.filter ~f:(fun p -> String.length p > 0) )
  | _ -> raise (RequestError "Header line is invalid")

let parse_header_line line =
  match String.lsplit2 line ~on:':' with
  | Some (key, value) ->
      Stdlib.print_endline key;
      (String.strip key, String.strip value)
  | None ->
      raise (RequestError (Stdlib.Printf.sprintf "Illegal header line %s" line))

let read_request_line input =
  let rec read_request_line_ input prev_char acc =
    let* char = Lwt_io.read_char input in
    match (prev_char, char) with
    | Some '\r', '\n' ->
        Lwt.return
          (String.of_char_list
             (List.sub (List.rev acc) ~pos:0 ~len:(List.length acc - 1)))
    | _, char -> read_request_line_ input (Some char) (char :: acc)
  in
  read_request_line_ input None []

let read_headers input =
  let rec read_headers_ input acc =
    let* line = read_request_line input in
    match line with
    | "" -> Lwt.return (List.rev acc)
    | line -> read_headers_ input (parse_header_line line :: acc)
  in
  read_headers_ input []

let read_content input content_length =
  let rec read_content_ input content_length acc =
    match content_length with
    | 0 -> Lwt.return (List.rev acc |> String.of_char_list)
    | _ ->
        let* char = Lwt_io.read_char input in
        read_content_ input (content_length - 1) (char :: acc)
  in
  read_content_ input content_length []

let read input =
  let* header_line = read_request_line input in
  let* headers = read_headers input in
  let content_length = content_length headers in
  let* content =
    match content_length with
    | Some content_length ->
        let* content = read_content input content_length in
        Lwt.return (Some content)
    | None -> Lwt.return None
  in
  let method_, path = parse_request_line header_line in
  Lwt.return { method_; path; headers; content }
