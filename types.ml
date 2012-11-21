type change_list =
  { file : string;
    changes : change list }
and change =
  { change_line : int;
    new_line : string; }

(*change_list is converted to commit*)
(*must stick thise module signature because of the conf type*)
module type Read = sig
  val parse_changes : lines:string list -> cwd:string -> change_list list
end

type commit = 
  { path : string;
    lines : string list; }

type preview =
  { line : int;
    minus_line : string;
    plus_line : string; }

type conf = 
  { input : string list;
    input_parser : (module Read);
    diff_out : minus_line:string -> plus_line:string -> string;
    action : [`Preview | `Commit ];
    cwd : string; }


