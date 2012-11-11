let autoreset = ref true

let set_autoreset b = autoreset := b

type color =
    Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

type attribute =
    Clear
  | Bold
  | Dark
  | Underline
  | Blink
  | Reverse
  | Concealed
  | Color of color
  | Background of color

let black = Color Black
let red = Color Red
let green = Color Green
let yellow = Color Yellow
let blue = Color Blue
let magenta = Color Magenta
let cyan = Color Cyan
let white = Color White

let on_black = Background Black
let on_red = Background Red
let on_green = Background Green
let on_yellow = Background Yellow
let on_blue = Background Blue
let on_magenta = Background Magenta
let on_cyan = Background Cyan
let on_white = Background White

let color_code = function
    Black       -> "30"
  | Red         -> "31"
  | Green       -> "32"
  | Yellow      -> "33"
  | Blue        -> "34"
  | Magenta     -> "35"
  | Cyan        -> "36"
  | White       -> "37"

let background_code = function
    Black       -> "40"
  | Red         -> "41"
  | Green       -> "42"
  | Yellow      -> "43"
  | Blue        -> "44"
  | Magenta     -> "45"
  | Cyan        -> "46"
  | White       -> "47"

let code = function
    Clear       -> "0"
  | Bold        -> "1"
  | Dark        -> "2"
  | Underline   -> "4"
  | Blink       -> "5"
  | Reverse     -> "7"
  | Concealed   -> "8"
  | Color c     -> color_code c
  | Background c -> background_code c

let attr_to_string attr = "[" ^ code attr ^ "m"

let apply attrs s =
    String.concat "" (List.map attr_to_string attrs)
  ^ s ^ (if !autoreset then "[0m" else "")
