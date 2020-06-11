open Streaming

let wc_l path =
  path
  |> Stream.file
  |> Stream.split ~by:(fun x -> x = '\n')
  |> Stream.len


let baseline_rec path =
  let ic = open_in path in
  let rec loop n =
    match input_char ic with
    | c when c = '\n' -> loop (n + 1)
    | _ -> loop n
    | exception End_of_file -> n in
  loop 0


let () =
  try
    print_endline (string_of_int (wc_l Sys.argv.(1)))
  with Invalid_argument _ -> prerr_endline "usage: wc <path>"

