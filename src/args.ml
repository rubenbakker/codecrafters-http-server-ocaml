open! Base

type options_t = { directory : string option }

let rec make_pairs (input : 'a list) : ('a * 'a) list =
  match input with
  | [] -> []
  | _ :: [] -> []
  | hd :: second :: tl -> (hd, second) :: make_pairs tl

let args_only (argv : string Array.t) : (string * string) list =
  Array.sub ~pos:1 ~len:(Array.length argv - 1) argv
  |> Array.to_list |> make_pairs

let value_of_arg (argv : string Array.t) (key : string) : string option =
  List.Assoc.find (args_only argv) ~equal:String.equal key

let parse_options (argv : string Array.t) =
  { directory = value_of_arg argv "--directory" }
