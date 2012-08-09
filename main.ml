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
