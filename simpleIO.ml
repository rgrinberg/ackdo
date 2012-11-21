
let read_lines_in chan = 
  let rec loop l = 
    try loop ((input_line chan) :: l)
    with End_of_file -> close_in chan; List.rev l
  in loop []

let read_lines path = 
  let chan = open_in path in
  read_lines_in chan

let write_lines path lines = 
  let oc = open_out path in
  List.iter (fun l -> 
    Printf.fprintf oc "%s\n" l;
  ) lines; close_out oc
