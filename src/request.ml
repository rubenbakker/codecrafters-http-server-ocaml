open Base

type method_t = Get | Head | Post | Put | Patch | Delete
type t = { method_ : method_t; path : string }

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

let parse_header_line line =
  match String.split line ~on:' ' with
  | [ method_; path; _ ] -> { method_ = convert_method method_; path }
  | _ -> raise (RequestError "Header line is invalid")

let read input =
  let* header_line = Lwt_io.read_line_opt input in
  match header_line with
  | None -> raise (RequestError "No request found")
  | Some header_line -> Lwt.return (parse_header_line header_line)
