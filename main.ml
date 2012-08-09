open Ackdo
open Batteries

let () =
  try begin
    let cfg = CmdArgs.read_args () in
    Operations.run_program cfg;
  end
  with 
  | InputDetector.Failed_to_detect -> 
      print_endline "bad input from file or stdin";
  | Sys_error(_) ->
      print_endline "most likely bad input file provided but who knows we aren't
handling this case thoroughly";

(*
 *the options this program takes are:
 *  1. the file with the edited ack output (optional)
 *  2. edited ack output from stdin
 *  3. cwd (optional, assumed to be either user's cwd or the cwd of the passed
 *  file) or can be specified
 *)

(*
 *there should be two options of operations:
 *1. preview changes
 *2. commit changes
 *)
