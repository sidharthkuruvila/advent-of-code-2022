open Core


   
let small_input = {|[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]|}

type token =
  | O
  | C
  | N of int

let char_to_token c =
  match c with
  | "[" -> O
  | "]" -> C
  | c -> N (int_of_string c)
let section_split_re = Re.Perl.compile_pat "\n\n"
let re = Re.Perl.compile_pat "\\d+|\\[|\\]"
let parse s =
  let sections = Re.split section_split_re s in
  List.map sections ~f:(fun section ->
      let [@warning "-8"] [left; right] = String.split section ~on:'\n'
                           |> List.map ~f:(fun line -> line
                                                       |>  Re.all re
                                                       |> List.map ~f:(fun m -> Re.Group.get m 0 |> char_to_token)) in
    (left, right))
    
               
let compare (left, right) =
  let rec loop left right = 
  match (left, right) with
  | (O::restl, O::restr) -> loop restl restr
  | (C::restl, C::restr) -> loop restl restr
  | (O::_, N i::restr) -> loop left (O::(N i)::C::restr)
  | (N i::restl, O::_) -> loop (O::N i::C::restl) right
  | (N a::restl, N b::restr) when a = b -> loop restl restr
  | (N a::_, N b::_)  -> a < b
  | (_::_, C::_) -> false
  | (C::_, _::_) -> true
  | ([], _) -> true
  | (_, []) -> false in
  loop left right
 
              
let part_1 s =
  let res = parse s
            |> List.map ~f:compare
            |> List.mapi ~f:(fun i a -> if a then (i + 1) else 0)
            |> List.sum (module Int) ~f:Fn.id in
  Printf.printf "Res: %d\n" res

let prefix = {|[[2]]
[[6]]

|}

              
let part_2 s =
  let res = parse (prefix ^ s)
            |> List.concat_map ~f:(fun (a, b) -> [a; b])
            |> List.sort ~compare:(fun a b -> if compare (a, b) then -1 else 1 )
            |> List.filter_mapi ~f:(fun i a ->
                   match a with
                   | [O; O; N 2; C; C] | [O; O; N 6; C; C] -> Some (i + 1)
                   | _ -> None)
          
            |> List.fold_left ~init:1 ~f:( * ) in
  Printf.printf "Res: %d\n" res
                
let big_input = In_channel.read_all "day13/input.txt" |> (String.strip ~drop:Char.(fun c -> c = '\n'))

  
 let _ =
   print_endline "Part 1:";
   print_endline "Small Input";
   part_1 small_input;
   print_endline "Big Input";
   part_1 big_input;
   print_endline "Part 2:";
   print_endline "Small Input";
   part_2 small_input;
   print_endline "Big Input";
   part_2 big_input;
