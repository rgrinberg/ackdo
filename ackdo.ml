let (|>) g f = f g
let (-|) g f = fun x -> x |> f |> g 

(*let () = Printexc.record_backtrace true*)
module List = struct
  (*
   *we would like a slim list of dependencies, hence we have to reinvent a few
   *wheels here
   *)
  include List

  let map_by_two f l = 
    let rec loop acc = function
      | x::y::xs -> loop ((f x y)::acc) xs
      | _::[] -> failwith "Odd number of elements"
      | [] -> List.rev acc
    in loop [] l

  let take x l = 
    let rec loop acc count l =
      if count = 0 then List.rev acc
      else match l with
      | x::xs -> loop (x::acc) (pred count) xs
      (*
       *if there are not enough elements we return what we have instead
       *of throwing an error
       *)
      | [] -> List.rev acc
    in loop [] x l

  (* compatibility for 3.12 *)
  let mapi f a = 
    let rec loop acc i = function
      | [] -> List.rev acc
      | x::xs -> loop ((f i x)::acc) (succ i) xs
    in loop [] 0 a

  let group_by f l = 
    let rec loop acc current_acc last = function
      | (x::xs) as l ->
          begin match last with
          | Some (last_e) -> 
              if (f x last_e)
              then loop acc (x::current_acc) (Some x) xs
              else loop ((List.rev current_acc)::acc) [] None l
          | None -> loop acc [x] (Some x) xs
          end
      | [] -> List.rev ((List.rev current_acc)::acc)
    in match l with 
    | [] -> []
    | xs -> loop [] [] None xs
end

module String = struct
  include String
  let split s ~by = 
    if by = "" then (s, "") else begin
      let re = Str.regexp ("\(^.*\)" ^ (Str.quote by) ^ "\(.*\)$") in
      let smatch = Str.string_match re s 0 in
      if not smatch then raise Not_found
      else Str.(matched_group 1 s, matched_group 1 s)
    end
end                             

module LCS = struct 
  (*inefficient crap. rewrite later*)
  let longest xs ys = if List.length xs > List.length ys then xs else ys

  let list_of_string str =
    let result = ref [] in
    String.iter (fun x -> result := x :: !result)
    str; List.rev !result
 
  let string_of_list lst =
    let result = String.create (List.length lst) in
    ignore (List.fold_left (fun i x -> result.[i] <- x; i+1) 0 lst);
    result

  let lcs' xs' ys' =
    let (xs, ys) = Array.(of_list xs', of_list ys') in
    let (n, m) = Array.(length xs, length ys) in
    let a = Array.make_matrix (n+1) (m+1) [] in
    for i = n-1 downto 0 do
      for j = m-1 downto 0 do
        a.(i).(j) <- if xs.(i) = ys.(j) then xs.(i) :: a.(i+1).(j+1)
                     else longest a.(i).(j+1) a.(i+1).(j)
      done
    done;
    a.(0).(0)

  (*NOTE: this lcs is actually lowest common subsequence not string. fix later*)
  let lcs xs ys = 
    (lcs' (list_of_string xs) (list_of_string ys)) |> string_of_list
end

module Misc = struct
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
end


type change_list =
  { file : string;
    changes : change list }
and change =
  { change_line : int;
    new_line : string; }
(*change_list is converted to commit*)
type commit = 
  { path : string;
    lines : string list; }
(*
 *we must stick this module declaration here for now until I learn how
 *to define types and module signatures recursively
 *)
module type Read = sig
  val parse_changes : lines:string list -> cwd:string -> change_list list
end
type preview =
  { line : int;
    minus_line : string;
    plus_line : string; }

(*contains all the necessary information needed to run the program*)
type conf = 
  { input : string list;
    input_parser : (module Read);
    diff_out : minus_line:string -> plus_line:string -> string;
    action : action;
    cwd : string; }
and action = Preview | Commit

exception File_does_not_exist of string


module Diffs = struct
  (* TODO : haven't implement color diff yet *)
  let color_line x lcs y = 
    let open ANSIColor in x ^ (apply [red] lcs) ^ y
  let color ~minus_line ~plus_line = 
    let lcs = LCS.lcs minus_line plus_line in
    let (m1, m2) = String.split minus_line ~by:lcs in
    let (p1, p2) = String.split plus_line ~by:lcs in
    "-" ^ (color_line m1 lcs m2) ^ "\n+" ^ (color_line p1 lcs p2)

  let black_white ~minus_line ~plus_line =
    "- " ^ minus_line ^ "\n+ " ^ plus_line
end

module Display = struct
  let display_diffs ~file ~diffs ~diff_out =
    print_endline ("--> " ^ file);
    match diffs with
    | [] -> print_endline "[0 changes]";
    | _ -> diffs |> List.iter (fun { line; minus_line; plus_line } ->
        print_endline ((string_of_int line) ^ ":");
        print_endline (diff_out ~minus_line ~plus_line))
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
      let new_lines = file |> Misc.read_lines
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
    Misc.write_lines path lines
  let write_all_changes cg = cg |> List.iter write_changes
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
    let unmerged = l |> List.group_by (fun a b -> (f a) = (f b))
    in unmerged |> List.map_by_two (fun x y -> (List.hd x, y))

  let parse_change = 
    let re = Str.regexp "^\([0-9]+\):\(.+\)$" in
    (fun line -> 
      (Str.string_match re line 0) |> ignore;
      let open Str in
      { change_line=((matched_group 1 line) |> int_of_string );
        new_line=(matched_group 2 line) })

  let parse_changes ~lines ~cwd = lines
    |> List.filter (not -| StrMisc.blank_str) 
    |> split_f (not -| StrMisc.grouped_match)
    |> List.map ( fun (f, changes) ->
       let file = Filename.concat cwd f in
       { file; changes=(changes |> List.map parse_change) })
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
    |> List.filter (not -| StrMisc.blank_str)
    |> List.map parse_change
    |> List.group_by (fun (a,_) (b,_) -> a = b)
    |> List.map (fun e ->
        let file = Filename.concat cwd
          (match e with 
          | (f,_)::_ -> f
          | [] -> failwith "wtf this is bullshit")
        in { file; changes=(e |> List.map snd) })
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
    then (module Ungrouped : Read)
    else match e with
      | File::Line::_ -> (module Grouped : Read)
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
    let change_lists = 
      let module IP = (val input_parser : Read) in
      (IP.parse_changes ~lines ~cwd) in
    try
      change_lists
      |> List.map ( fun { file; _ } -> file ) |> verify_paths;
      let changes = change_lists |> List.map Commit.file_of_changes in
      match action with
      | Preview -> changes |> List.iter ( fun ({path=file;_}, preview_list) ->
          Display.display_diffs ~file ~diffs:preview_list ~diff_out );
      | Commit -> 
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
    let action     = ref Preview in
    let input      = ref [] in
    let cwd        = ref (Unix.getcwd ()) in
    let forced_cwd = ref false in
    let printer    = ref Diffs.black_white in
    let speclist = [
      ("-d", Arg.Unit ( fun () -> action := Commit ), ": -d to commit. nothing
      to preview" );

      ("-f", Arg.String ( fun s -> 
        if not (Sys.file_exists s) then
          raise (Arg.Bad ("File doesn't exist:" ^ s))
        else begin
          input := (Misc.read_lines s);
          input_file :=  Some(s);
        end
        ), ": -f read input from some file instead of stdin");

      ("-c", Arg.String (fun d -> cwd := d; forced_cwd := true ),
       ": force cwd to be argument" );

      ("-r", Arg.Unit ( fun () -> printer := Diffs.color ),
        ": turn on color output" );
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
    input := (stdin |> Misc.read_lines_in );
    (match (!input_file) with
    | Some f when (not (!forced_cwd)) -> cwd := Filename.dirname f
    | Some _ | None -> ());
    let open InputDetector in
    let module IP = (val (detect_input !input) : Read) in
    { action=(!action) ; input_parser=(module IP : Read); input=(!input) ;
      cwd=(!cwd) ; diff_out=(!printer) }
end
