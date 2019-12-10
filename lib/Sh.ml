open Streaming__Sink
open Streaming__Source
open Streaming__Stream


let rec waitpid flags pid = try Unix.waitpid flags pid with
  | Unix.Unix_error (Unix.EINTR, _, _) -> waitpid flags pid

let rec unix_read fd bytes i n = try Unix.read fd bytes i n with
  | Unix.Unix_error (Unix.EINTR, _, _) -> unix_read fd bytes i n

let rec create_process prog args ~stdin ~stdout ~stderr =
  try Unix.create_process prog args stdin stdout stderr with
  | Unix.Unix_error (Unix.EINTR, _, _) ->
      create_process prog args ~stdin ~stdout ~stderr


let read_process cmd =
  let init () = Unix.open_process_in cmd in
  let pull ic =
    try Some (input_line ic, ic)
    with End_of_file -> None in
  let stop = close_in in
  Source { init; pull; stop }



let pipe name args ?(env = []) self =
  let reduce (Sink k) =
    let (send_r, send_w) = Unix.pipe () in
    let recv_r = ExtUnix.All.posix_openpt [Unix.O_RDWR; Unix.O_NOCTTY] in
    ExtUnix.All.grantpt recv_r;
    ExtUnix.All.unlockpt recv_r;
    let recv_w_tty = ExtUnix.All.ptsname recv_r in
    let output_buf = Bytes.make 4096 '\000' in
    let feed str =
      let line = Bytes.of_string (str ^ "\n") in
      let line_len = Bytes.length line in
      let wn = Unix.write send_w line 0 line_len in
      if wn <> line_len then prerr_endline "incorrect write to pipe";
      let rn = Unix.read recv_r output_buf 0 99 in
      Bytes.to_string (Bytes.sub output_buf 0 (rn - 2)) in
    match Unix.fork () with
    | 0 ->
      Unix.close send_w;
      Unix.close recv_r;

      (* Setup pipe redirect for STDIN. *)
      Unix.close Unix.stdin;
      Unix.dup2 send_r Unix.stdin;
      Unix.close send_r;

      (* Setup tty redirect for STDOUT. *)
      let recv_w = Unix.openfile recv_w_tty [Unix.O_RDWR; Unix.O_NOCTTY] 0o640 in
      Unix.close Unix.stdout;
      Unix.dup2 recv_w Unix.stdout;
      Unix.close recv_w;
      Unix.execve name (Array.of_list (name :: args)) (Array.of_list env)
    | pid ->
      (* Close unused pipe ends. *)
      Unix.close send_r;
      let step acc x = (k.step acc (feed x)) in
      let stop acc =
        Unix.kill pid Sys.sigint;
        let _ = waitpid [] pid in
        Printf.eprintf "[TRACE] Killed %d\n%!" pid;
        k.stop acc in
      self.reduce (Sink { k with step; stop }) in
  { reduce }


let grep = pipe "/usr/bin/grep"

