let create_matcher re = 
  let r = Str.regexp re in
  ( fun x -> Str.string_match r x 0 )
let blank_str = create_matcher "^ *$"
let grouped_match = create_matcher "^[1-9][0-9]*:.+$" 
let ungrouped_detect = create_matcher "^.+:[0-9]+:.+$" 
let grouped_filepath = create_matcher "^.+$"
