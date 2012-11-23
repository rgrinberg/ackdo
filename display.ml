open Types
open Common

let display_diffs ~file ~diffs ~diff_out =
  print_endline ("--> " ^ ANSIColor.(apply [green] file));
  match diffs with
  | [] -> print_endline "[0 changes]";
  | _ -> diffs |> List.iter (fun { line; minus_line; plus_line } ->
      (*let line = int_of_string line in*)
      print_endline (diff_out ~line ~minus_line ~plus_line))
