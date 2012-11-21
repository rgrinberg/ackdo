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
