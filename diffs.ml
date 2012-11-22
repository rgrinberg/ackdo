let print_line_num ~line = print_endline ((string_of_int line) ^ ":")

(* TODO : haven't implement color diff yet *)
let color_line x lcs y = 
  let open ANSIColor in x ^ (apply [red] lcs) ^ y

let black_white ~line ~minus_line ~plus_line =
  print_line_num ~line;
  "- " ^ minus_line ^ "\n+ " ^ plus_line

let color ~line ~minus_line ~plus_line = 
  black_white ~line ~minus_line ~plus_line
  (*let lcs = LCS.lcs minus_line plus_line in*)
  (*let (m1, m2) = String.split minus_line ~by:lcs in*)
  (*let (p1, p2) = String.split plus_line ~by:lcs in*)
  (*"-" ^ (color_line m1 lcs m2) ^ "\n+" ^ (color_line p1 lcs p2)*)

let app3 f (x,y,z) = (f x, f y, f z)

let replace_str ~table ~str = 
  List.fold_left (fun s (rule, replace) ->
    Str.global_replace rule replace s ) str table

let custom_diff ~format = 
  (*let (re_m, re_p, re_l) = Str.(regexp "%\-", regexp "%\+") in*)
  let (re_m, re_p, re_l) = app3 Str.regexp ("%\-","%\+","%l") in
  begin fun ~line ~minus_line ~plus_line ->
    let rep_tbl = [
      (re_m, minus_line);
      (re_p, plus_line);
      (re_l, (string_of_int line));
    ] in replace_str ~table:rep_tbl ~str:format
  end
