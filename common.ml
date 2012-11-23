let (|>) g f = f g
let (-|) g f = fun x -> x |> f |> g 

module List   = ListExt
module String = StringExt

