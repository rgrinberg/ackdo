(** ANSIColor is a simple library for coloring strings in a terminal with
    ANSI colors. *)

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

(** convenience values, to save typing: *)

val black : attribute
val red : attribute
val green : attribute
val yellow : attribute
val blue : attribute
val magenta : attribute
val cyan : attribute
val white : attribute

val on_black : attribute
val on_red : attribute
val on_green : attribute
val on_yellow : attribute
val on_blue : attribute
val on_magenta : attribute
val on_cyan : attribute
val on_white : attribute

(** [apply attrs s] applies [attrs], in order, to [s].  If autoreset is
    OFF, any following text will also have [attrs] applied to it.  *)
val apply : attribute list -> string -> string

(** Turns the autoreset feature on and off.  It defaults to on. *)
val set_autoreset : bool -> unit
