
include String
let split s ~by = 
  if by = "" then (s, "") else begin
    let re = Str.regexp ("\(^.*\)" ^ (Str.quote by) ^ "\(.*\)$") in
    let smatch = Str.string_match re s 0 in
    if not smatch then raise Not_found
    else Str.(matched_group 1 s, matched_group 1 s)
  end
