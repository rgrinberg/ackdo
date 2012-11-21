(* TODO : haven't implement color diff yet *)
let color_line x lcs y = 
  let open ANSIColor in x ^ (apply [red] lcs) ^ y

let black_white ~minus_line ~plus_line =
  "- " ^ minus_line ^ "\n+ " ^ plus_line

let color ~minus_line ~plus_line = 
  black_white ~minus_line ~plus_line
  (*let lcs = LCS.lcs minus_line plus_line in*)
  (*let (m1, m2) = String.split minus_line ~by:lcs in*)
  (*let (p1, p2) = String.split plus_line ~by:lcs in*)
  (*"-" ^ (color_line m1 lcs m2) ^ "\n+" ^ (color_line p1 lcs p2)*)

let custom_diff ~format = 
  let (re_m, re_p) = Str.(regexp "%\-", regexp "%\+") in
  begin fun ~minus_line ~plus_line ->
    let tmp = Str.global_replace re_m minus_line format in
    let tmp = Str.global_replace re_p plus_line tmp in
    tmp
  end
