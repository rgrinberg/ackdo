open Batteries

type change_list =
  { file : string;
    changes : change Enum.t }
and change =
  { change_line : int;
    new_line : string; }
type commit = 
  { path : string;
    lines : string Enum.t; }
(*
 *we must stick this module declaration here for now until I learn how to define
 *types and module signatures recursively
 *)
module type Read = sig
  val parse_changes : lines:string Enum.t -> cwd:string -> change_list Enum.t
end
type preview =
  { line : int;
    minus_line : string;
    plus_line : string; }

(*contains all the necessary information needed to run the program*)
type conf = 
  { input : string Enum.t;
    input_parser : (module Read);
    action : action;
    cwd : string; }
and action = Preview | Commit

exception File_does_not_exist of string

module Display = struct
  let diff_out ~minus_line ~plus_line = "- " ^ minus_line ^ "\n+ " ^ plus_line
  let display_diffs ~file ~diffs =
    print_endline ("--> " ^ file);
    diffs |> Enum.iter (fun { line; minus_line; plus_line } ->
      print_endline ((string_of_int line) ^ ":");
      print_endline (diff_out ~minus_line ~plus_line))
end

module Commit = struct 
  (*
   *converts a 'change list' into a Hashtbl that makes it easy to access changes
   *corresponding to specific lines and remove changes once they are committed 
   *)
  let prepare_changes changes =
    let chash = Hashtbl.create 10 in
    changes |> Enum.iter ( fun { change_line; new_line } ->
      if Hashtbl.mem chash change_line
      then failwith "Cannot have duplicate line edits on same file"
      else Hashtbl.add chash change_line new_line
    ); chash
  (*
   *file_of_changes returns an Enum of new lines that are to be written to
   *commit. "changes"not lazy, will actually open the file and read from it
   *)
  let file_of_changes { file ; changes } = 
    if Sys.file_exists file
    then
      let previews = ref [] in
      let change_hash = prepare_changes changes in
      let new_lines = file |> BatFile.lines_of
        |> Enum.mapi ( fun line_number_minus_one old_line -> 
            (*line_number_minus_one starts counting at 0 so we must
             * increment it*)
            let line_number = succ line_number_minus_one in
            try begin
              let new_line = Hashtbl.find change_hash line_number in
              previews := { line=line_number;
                            minus_line=old_line;
                            plus_line=new_line; } :: !previews;
              new_line
            end with Not_found -> old_line ) |> List.of_enum
      in ({ path=file; lines=(List.enum new_lines) }
            ,List.rev !previews)
    else raise (File_does_not_exist file)
  (*warning: untested*)
  let write_changes { path ; lines } = BatFile.write_lines path lines
  let write_all_changes cg = cg |> Enum.iter write_changes
end

module StrMisc = struct
  let blank_str =
    let re = Str.regexp "^ *$" in
    (fun x -> Str.string_match re x 0)
end

module Grouped : Read = struct 
  let is_line = 
    let re = Str.regexp "^[0-9]+:.+$" in
    (fun x -> Str.string_match re x 0)

  let split_f f l =
    let unmerged = l |> Enum.group_by (fun a b -> (f a) = (f b))
     in Enum.from (fun () ->
       match (Enum.get unmerged, Enum.get unmerged) with
       | Some(x),Some(y) -> (Enum.get_exn x, y)
       | None, None -> raise BatEnum.No_more_elements
       | Some(_), None -> failwith "Odd number of elements. This is bullshit."
       | None, Some(_) -> failwith "broke the matrix")

  let parse_change = 
    let re = Str.regexp "^\([0-9]+\):\(.+\)$" in
    (fun line -> 
      (Str.string_match re line 0) |> ignore;
      let open Str in
      { change_line=((matched_group 1 line) |> int_of_string );
        new_line=(matched_group 2 line) })

  let parse_changes ~lines ~cwd = lines
    |> Enum.filter (not -| StrMisc.blank_str) 
    |> split_f (not -| is_line)
    |> Enum.map ( fun (f, changes) ->
       let file =  Filename.concat cwd f in
       { file; changes=(changes |> Enum.map parse_change) })
end

module Ungrouped : Read = struct
  let parse_change =
    let re = Str.regexp "^\(.+\):\([0-9]+\):\(.+\)$" in
    (fun line ->
      (Str.string_match re line 0) |> ignore;
      let open Str in
      ( matched_group 1 line,
      { change_line=((matched_group 2 line) |> int_of_string );
        new_line=(matched_group 3 line) }) )

  let parse_changes ~lines ~cwd = lines
    |> Enum.filter (not -| StrMisc.blank_str)
    |> Enum.map parse_change
    |> Enum.group_by (fun (a,_) (b,_) -> a = b)
    |> Enum.map (fun e ->
        let file = Filename.concat cwd
          (match (Enum.peek e) with 
          | Some(f, _) -> f
          | None -> failwith "wtf this is bullshit")
        in { file; changes=(e |> Enum.map snd) })
end

module Operations = struct
  (*
   *this module does what the user wants and prints the results. However it does
   *not read in the user's options and assumes those settings have been passed
   *down
   *)
  exception Bad_paths of string list
  let verify_paths paths =
    match paths |> Enum.filter (not -| Sys.file_exists ) |> List.of_enum with
    | [] -> () | xs -> raise (Bad_paths xs)

  let run_program { input=lines ; input_parser ; action ; cwd } = 
    let module IP = (val input_parser : Read) in
    let change_lists = IP.parse_changes ~lines ~cwd in
    try
      change_lists |> Enum.map ( fun { file; _ } -> file ) |> verify_paths;
      let changes = change_lists |> Enum.map Commit.file_of_changes in
      match action with
      | Preview -> changes |> Enum.iter ( fun ({path=file;_}, preview_list) ->
            Display.display_diffs ~file ~diffs:(List.enum preview_list) )
      | Commit -> changes |> Enum.map fst |> Commit.write_all_changes
    with Bad_paths(p) -> begin
      print_endline "looks like you supplied bad paths. perhaps cwd is wrong?";
      p |> List.iter ( fun path -> Printf.printf "'%s' does not exist\n" path )
    end
end

