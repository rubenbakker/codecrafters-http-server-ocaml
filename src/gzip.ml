open! Base

let w = De.make_window ~bits:15
let l = De.Lz77.make_window ~bits:15
let o = De.bigstring_create De.io_buffer_size
let i = De.bigstring_create De.io_buffer_size
let q = De.Queue.create 4096
let str fmt = Stdlib.Format.asprintf fmt
let msgf fmt = Stdlib.Format.kasprintf (fun msg -> `Msg msg) fmt
let error_msgf fmt = Stdlib.Format.kasprintf (fun err -> Error (`Msg err)) fmt

let bigstring_input ic buf off len =
  let tmp = Bytes.create len in
  try
    let len = Stdlib.input ic tmp 0 len in
    for i = 0 to len - 1 do
      buf.{off + i} <- Bytes.get tmp i
    done;
    len
  with End_of_file -> 0

let bigstring_output oc buf off len =
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.set res i buf.{off + i}
  done;
  Stdlib.output_string oc (Stdlib.Bytes.unsafe_to_string res)

let decompress_channel ic oc =
  let open Gz in
  let decoder = Inf.decoder `Manual ~o in

  let rec go decoder =
    match Inf.decode decoder with
    | `Await decoder ->
        let len = bigstring_input ic i 0 io_buffer_size in
        Inf.src decoder i 0 len |> go
    | `Flush decoder ->
        let len = io_buffer_size - Inf.dst_rem decoder in
        bigstring_output oc o 0 len;
        Inf.flush decoder |> go
    | `Malformed err -> `Error (false, str "%s." err)
    | `End decoder ->
        let len = io_buffer_size - Inf.dst_rem decoder in
        if len > 0 then bigstring_output oc o 0 len;
        `Ok 0
  in
  go decoder

let now () = Int32.of_float (Unix.gettimeofday ())

let compress_channel ~level ic oc =
  let open Gz in
  let encoder =
    Def.encoder `Manual `Manual ~q ~w:l ~level ~mtime:(now ()) Gz.Unix
  in

  let rec go encoder =
    match Def.encode encoder with
    | `Await encoder ->
        let len = bigstring_input ic i 0 io_buffer_size in
        Def.src encoder i 0 len |> go
    | `Flush encoder ->
        let len = io_buffer_size - Def.dst_rem encoder in
        bigstring_output oc o 0 len;
        Def.dst encoder o 0 io_buffer_size |> go
    | `End encoder ->
        let len = io_buffer_size - Def.dst_rem encoder in
        if len > 0 then bigstring_output oc o 0 len;
        `Ok 0
  in
  Def.dst encoder o 0 io_buffer_size |> go

let compress_string input =
  let _ic = Redirect.with_channel_from_string input in
  ()

(* let run deflate level filename_ic filename_oc = *)
(*   let ic, close_ic = *)
(*     match filename_ic with *)
(*     | Some filename -> *)
(*         let ic = open_in_bin filename in *)
(*         (ic, fun () -> close_in ic) *)
(*     | None -> (stdin, ignore) *)
(*   in *)
(*   let oc, close_oc = *)
(*     match filename_oc with *)
(*     | Some filename -> *)
(*         let oc = open_out_bin filename in *)
(*         (oc, fun () -> close_out oc) *)
(*     | None -> (stdout, ignore) *)
(*   in *)
(*   let res = *)
(*     match (deflate, format) with *)
(*     | true, `Deflate -> run_deflate ~level ic oc *)
(*     | false, `Deflate -> run_inflate ic oc *)
(*     | true, `Zlib -> run_zlib_deflate ~level ic oc *)
(*     | false, `Zlib -> run_zlib_inflate ic oc *)
(*     | true, `Gzip -> run_gzip_deflate ~level ic oc *)
(*     | false, `Gzip -> run_gzip_inflate ic oc *)
(*     | true, `Lzo -> run_lzo_deflate ic oc *)
(*     | false, `Lzo -> run_lzo_inflate ic oc *)
(*   in *)
(*   close_ic (); *)
(*   close_oc (); *)
(*   res *)
