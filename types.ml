type change_list =
  { file : string;
    changes : change list }
and change =
  { change_line : int;
    new_line : string; }

(*change_list is converted to commit*)
(*must stick thise module signature because of the conf type*)
type parse_changes = lines:string list -> cwd:string -> change_list list

type commit = 
  { path : string;
    lines : string list; }

type preview =
  { line : int;
    minus_line : string;
    plus_line : string; }

(*
 *everything that is needed to run the program should be contained in this
 *type
 *)
type conf = 
  { input : string list;
    input_parser : parse_changes;
    diff_out : line:int -> minus_line:string -> plus_line:string -> string;
    action : [`Preview | `Commit ];
    cwd : string; }

exception File_does_not_exist of string
