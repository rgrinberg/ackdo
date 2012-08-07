open Ackdo
open Batteries

let pp_file ~file ~parser_f = 
  let changes = parser_f
                ~lines:(BatFile.lines_of file)
                ~cwd:"." in
  let cwd = Unix.getcwd () in
  print_endline file;
  print_endline "---------------------";
  changes |> Enum.iter ( fun { file ; changes } -> 
    Printf.printf "--> %s\n" file;
    changes |> Enum.iter ( fun { change_line ; new_line } ->
      Printf.printf "%d:%s\n" change_line new_line) )

let () = 
  let file = "/home/rudi/prog/ocaml/ackdo/testout.txt" in
  pp_file ~file ~parser_f:Grouped.parse_changes

let () = 
  let file = "/home/rudi/prog/ocaml/ackdo/testout2.txt" in
  pp_file ~file ~parser_f:Ungrouped.parse_changes

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
