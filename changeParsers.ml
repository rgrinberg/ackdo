open Common
open Types

(*this module is not really necessary*)
module type Read = sig val parse_changes : parse_changes end

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
  (*
   *We assume that all of the following characters cannot occur in the filepath.
   *This is of course a false assumption for Unix but it makes the change matcher
   *a lot more robust in practice. It also works well enough for real world
   *applications
   *)
  let disallowed_chars = Str.quote "()\"'"
  let parse_change =
    let re = Str.regexp ("^\([^"^ disallowed_chars ^"]+\):\([0-9]+\):\(.+\)$") in
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



