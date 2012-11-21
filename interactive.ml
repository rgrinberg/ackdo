let opts = [('y', `Write);('n', `Skip); ('a', `WriteAll); ('s', `SkipAll);
   ('x', `Abort)]

let action key = opts |> List.assoc key

let string_of_action = function
  | `Write    -> "write change"
  | `Skip     -> "skip change"
  | `WriteAll -> "write remaining"
  | `SkipAll  -> "skip remaining"
  | `Abort    -> "abort changes"

let dialog = 
  let opt (key, act) = Printf.sprintf "(%c) %s" key (string_of_action act) in
  let opts = opts |> List.map opt in
  let dlg = List.fold_right (fun e msg -> e ^ "/" ^ msg) opts "" in
  (fun () -> print_string dlg; (action (Console.get1char ())))

(*ripped off from stackoverflow*)
let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN 
  { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  let () = Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio in res
