open Ackdo
open Batteries

let () =
  try begin
    let cfg = CmdArgs.read_args () in
    Operations.run_program cfg;
  end
  with 
  | InputDetector.Failed_to_detect(x) -> 
      print_endline "terrible input from file or stdin";
      Printf.printf "Error: %s\n" x;
  | Sys_error(_) ->
      print_endline "most likely terrible input file provided but who knows we
aren't handling this case thoroughly";
