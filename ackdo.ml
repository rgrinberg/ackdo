open Common
open Types

(*let () = Printexc.record_backtrace true*)

(*contains all the necessary information needed to run the program*)

module Display = struct
  let display_diffs ~file ~diffs ~diff_out =
    print_endline ("--> " ^ ANSIColor.(apply [green] file));
    match diffs with
    | [] -> print_endline "[0 changes]";
    | _ -> diffs |> List.iter (fun { line; minus_line; plus_line } ->
        (*let line = int_of_string line in*)
        print_endline (diff_out ~line ~minus_line ~plus_line))
end

module Commit = struct 
  (*
   *converts a 'change list' into a Hashtbl that makes it easy to access
   *changes corresponding to specific lines and remove changes once they
   *are committed
   *)
  let prepare_changes changes =
    let chash = Hashtbl.create 10 in
    changes |> List.iter ( fun { change_line; new_line } ->
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
      let new_lines = file |> SimpleIO.read_lines
        |> List.mapi ( fun line_number_minus_one old_line -> 
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
            end with Not_found -> old_line )
      in ({ path=file; lines=new_lines }
            ,List.rev !previews)
    else raise (File_does_not_exist file)
  let write_changes { path ; lines } =
    SimpleIO.write_lines path lines
  let write_all_changes cg = cg |> List.iter write_changes
end


module InputDetector = struct
  type input_match = Full | File | Line | Unknown
  let string_of_match = function
    | File    -> "file"
    | Full    -> "full"
    | Line    -> "line"
    | Unknown -> "unknown"

  exception Failed_to_detect of string
  
  let line_marker line =
    let open StrMisc in
    if ungrouped_detect line then Full
    else if grouped_match line then Line
    else if grouped_filepath line then File
    else Unknown

  let detect_input input =
    let e = input |> List.filter (not -| StrMisc.blank_str)
                  |> List.take 10 |> List.map line_marker in
    if e |> List.for_all ( fun x -> x = Full ) 
    then ChangeParsers.Ungrouped.parse_changes
    else match e with
      | File::Line::_ -> ChangeParsers.Grouped.parse_changes
      | [] -> raise (Failed_to_detect "Received empty list, what gives?")
      | _ -> raise (Failed_to_detect "Bad input")
end

module Operations = struct
  (*
   *this module does what the user wants and prints the results. However
   *it does not read in the user's options and assumes those settings
   *have been passed down
   *)
  exception Bad_paths of string list
  let verify_paths paths =
    match paths |> List.filter (not -| Sys.file_exists ) with
    | [] -> () | xs -> raise (Bad_paths xs)

  let run_program { input=lines ; input_parser ; action ; cwd ; diff_out } = 
    let change_lists = input_parser ~lines ~cwd in
    try
      change_lists
      |> List.map ( fun { file; _ } -> file ) |> verify_paths;
      let changes = change_lists |> List.map Commit.file_of_changes in
      match action with
      | `Preview -> changes |> List.iter ( fun ({path=file;_}, preview_list) ->
          Display.display_diffs ~file ~diffs:preview_list ~diff_out );
      | `Commit -> 
        (*extract all the files that actually have changes and write those*)
        changes 
        |> List.filter ( fun (_,preview_l) -> (List.length preview_l) > 0 )
        |> List.map fst |> Commit.write_all_changes
    with Bad_paths(p) -> begin
      print_endline "looks like you supplied bad paths. perhaps cwd is wrong?";
      p |> List.iter ( fun path -> Printf.printf "'%s' does not exist\n" path )
    end
end

module CmdArgs = struct
  let usage = "usage: " ^ Sys.argv.(0) ^ " [-d] [-f file] [-c directory] [-r]"
  let read_args () = 
    let input_file = ref None in
    let action     = ref `Preview in
    let input      = ref [] in
    let cwd        = ref (Unix.getcwd ()) in
    let forced_cwd = ref false in
    let printer    = ref Diffs.black_white in
    let speclist = [
      ("-d", Arg.Unit ( fun () -> action := `Commit ), ": -d to commit. nothing
      to preview" );

      ("-f", Arg.String ( fun s -> 
        if not (Sys.file_exists s) then
          raise (Arg.Bad ("File doesn't exist:" ^ s))
        else begin
          input := (SimpleIO.read_lines s);
          input_file :=  Some(s);
        end
        ), ": -f read input from some file instead of stdin");

      ("-c", Arg.String (fun d -> cwd := d; forced_cwd := true ),
       ": force cwd to be argument" );

      ("-r", Arg.Unit ( fun () -> printer := Diffs.color ),
        ": turn on color output" );

      ("-s", Arg.String (fun format -> printer := Diffs.custom_diff ~format), 
        ": set custom formatter" );
     ] in
    Arg.parse 
      speclist 
      (fun x -> raise (Arg.Bad ("Bad argument : " ^ x)))
      usage;
    (*
     *we set the current directory to the parent of the input file's
     *path in the case when user specified an input file but did not set
     *an explicit cwd
     *)
    input := (stdin |> SimpleIO.read_lines_in );
    (match (!input_file) with
    | Some f when (not (!forced_cwd)) -> cwd := Filename.dirname f
    | Some _ | None -> ());
    let open InputDetector in
    let change_parser = detect_input !input in
    { action=(!action) ; input_parser=change_parser; input=(!input) ;
      cwd=(!cwd) ; diff_out=(!printer) }
end
