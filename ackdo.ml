open Batteries

let () = Printexc.record_backtrace true

type change_list =
  { file : string;
    changes : change Enum.t }
and change =
  { change_line : int;
    new_line : string; }
(*change_list is converted to commit*)
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
    match diffs |> Enum.peek with
    | None -> print_endline "[0 changes]";
    | Some(_) -> diffs |> Enum.iter (fun { line; minus_line; plus_line } ->
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
              if new_line <> old_line then begin
                previews := { line=line_number;
                              minus_line=old_line;
                              plus_line=new_line; } :: !previews;
                new_line end
              else old_line
            end with Not_found -> old_line ) |> List.of_enum
      in ({ path=file; lines=(List.enum new_lines) }
            ,List.rev !previews)
    else raise (File_does_not_exist file)
  (*warning: untested*)
  let write_changes { path ; lines } = BatFile.write_lines path lines
  let write_all_changes cg = cg |> Enum.iter write_changes
end

module StrMisc = struct
  let create_matcher re = 
    let r = Str.regexp re in
    ( fun x -> Str.string_match r x 0 )
  let blank_str = create_matcher "^ *$"
  let grouped_match = create_matcher "^[1-9][0-9]*:.+$" 
  let ungrouped_detect = create_matcher "^.+:[0-9]+:.+$" 
  let grouped_filepath = create_matcher "^.+$"
end

module Grouped : Read = struct 
  let split_f f l =
    let unmerged = l |> Enum.group_by (fun a b -> (f a) = (f b))
    in Enum.from (fun () ->
      match (Enum.get unmerged, Enum.get unmerged) with
      | Some(x),Some(y) -> (Enum.get_exn x, y)
      | None, None -> raise BatEnum.No_more_elements;
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
    |> split_f (not -| StrMisc.grouped_match)
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

module InputDetector = struct
  type input_match = Full | File | Line | Unknown
  let string_of_match = function
    | File -> "file"
    | Full -> "full"
    | Line -> "line"
    | Unknown -> "unknown"
  exception Failed_to_detect of string
  let line_marker line =
    let open StrMisc in
    if ungrouped_detect line then Full
    else if grouped_match line then Line
    else if grouped_filepath line then File
    else Unknown

  let detect_input input =
    (*we don't to want to "mutate" input*)
    let e = input |> Enum.clone |> Enum.filter (not -| StrMisc.blank_str)
                  |> Enum.take 10 |> Enum.map line_marker |> List.of_enum in
    (*let e = input |> Enum.clone |> Enum.filter (not -| StrMisc.blank_str)*)
                  (*|> Enum.take 10 |> List.of_enum in*)
    (*print_endline "printing list:::";*)
    (*e |> List.iter print_endline;*)
    (*let e = e |> List.map line_marker in*)
    if e |> List.for_all ( fun x -> x = Full ) 
    then (module Ungrouped : Read)
    else match e with
      | File::Line::xs -> (module Grouped : Read)
      | [] -> raise (Failed_to_detect "Received empty list, what gives?")
      | _ ->
          (*Printf.printf "len is: %d\n" (List.length e);*)
          (*e |> List.iter ( fun x -> print_endline (string_of_match x) );*)
          raise (Failed_to_detect "Bad input")
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
    let change_lists = 
      let module IP = (val input_parser : Read) in
      IP.parse_changes ~lines ~cwd in
    try
      change_lists |> Enum.clone 
      |> Enum.map ( fun { file; _ } -> file ) |> verify_paths;
      let changes = change_lists |> Enum.map Commit.file_of_changes in
      match action with
      | Preview -> changes |> Enum.iter ( fun ({path=file;_}, preview_list) ->
            Display.display_diffs ~file ~diffs:(List.enum preview_list) );
      | Commit -> 
        (*extract all the files that actually have changes and write those*)
        changes 
        |> Enum.filter ( fun (_,preview_l) -> (List.length preview_l) > 0 )
        |> Enum.map fst |> Commit.write_all_changes
    with Bad_paths(p) -> begin
      print_endline "looks like you supplied bad paths. perhaps cwd is wrong?";
      p |> List.iter ( fun path -> Printf.printf "'%s' does not exist\n" path )
    end
end

module CmdArgs = struct
  let usage = "usage: " ^ Sys.argv.(0) ^ " [-d] [-f file] [-c directory]"
  let read_args () = 
    let input_file = ref None in
    let action = ref Preview in
    let input = ref (IO.lines_of stdin) in
    let cwd = ref (Unix.getcwd ()) in
    let forced_cwd = ref false in
    let speclist = [
      ("-d", Arg.Unit ( fun () -> action := Commit ), ": -d to commit. nothing
      to preview" );

      ("-f", Arg.String ( fun s -> 
        if not (Sys.file_exists s) then
          raise (Arg.Bad ("File doesn't exist:" ^ s))
        else begin
          input := (BatFile.lines_of s);
          input_file :=  Some(s);
        end
        ), ": -f read
      input from some file instead of stdin");

      ("-c", Arg.String (fun d -> cwd := d; forced_cwd := true ),
       ": force cwd to be argument" )
     ] in
    Arg.parse 
      speclist 
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      usage;
    (*
     *we set the current directory to the parent of the input file's path in the
     *case when user specified an input file but did not set an explicit cwd
     *)
    (match (!input_file) with
    | Some f when (not (!forced_cwd)) -> cwd := Filename.dirname f
    | Some _ | None -> ());
    let open InputDetector in
    let module IP = (val (detect_input !input) : Read) in
    { action=(!action) ; input_parser=(module IP : Read); input=(!input) ;
      cwd=(!cwd) }
end
