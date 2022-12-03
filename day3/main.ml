open Core

let small_input = {|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw|}

let big_input = In_channel.read_all "day3/input.txt" |> String.strip
                
let split_string s =
   let sl = String.length s / 2 in
   let c1 =  String.sub s ~pos:0 ~len:sl in
   let c2 = String.sub s ~pos:sl ~len:sl in
   (c1, c2)
                
let parse s =
  String.split s ~on:'\n'
  |> List.map ~f:split_string
                
let find_common c1 c2 =
  let l1 = String.to_list c1 |> List.sort ~compare:Char.compare in
  let l2 = String.to_list c2 |> List.sort ~compare:Char.compare in
  let rec loop l1 l2 =
    let open Char in
    match (l1, l2) with
    | (x::_, y::_) when x = y -> x
                          | (x::xs, y::_) when x < y -> loop xs l2
                          | (_, _::ys) -> loop l1 ys
                          | _ -> failwith "Did not find any matching values" in
  loop l1 l2
  
let priority c =
  let open Char in
  if 'a' <= c && c <= 'z' then
    1 + (int_of_char c) - (int_of_char 'a')
  else if 'A' <= c && c <= 'Z' then
    27 + (int_of_char c) - (int_of_char 'A')
  else
    failwith "Char not in range"


let score_part_1 l =
  List.map l ~f:(fun (a, b) -> find_common a b |> priority)
  |> List.fold ~init:0 ~f:(+)

  
let part_1 input =
  let l = parse input in
  let res = score_part_1 l in
  Printf.printf "Res: %d\n" res
  

let parse_part_2 s =
  String.split s ~on:'\n'
  
let sack_to_int s =
  String.fold s ~init:0 ~f:(fun acc c -> (1 lsl priority c) lor acc)
  
let merge_2 l =
  List.cartesian_product l l
  |> List.filter_map ~f:(fun (a, b) -> if a >= b then None else Some (a, b, a land b))
  
let has_single_bit n =
  if n < 0 then failwith "Must be non negative"
  else if n = 0 then false
  else
    n land (n - 1) = 0
  
let merge_3 m2l l =
  List.cartesian_product m2l l
  |> List.filter_map ~f:(fun ((a, b, m2), c) ->
         if b >= c
         then None
         else
           let m3 = m2 land c in
           if has_single_bit m3 then
             Some (a, b, c, m3)
           else
             None)
            
let merge l =
  let m2 = merge_2 l in
  let m3 = merge_3 m2 l in
  m3
 let _ =
   print_endline "Part 1:";
   print_endline "Small Input";
   part_1 small_input;
   print_endline "Big Input";
   part_1 big_input(*;
   print_endline "Part 2:";
   print_endline "Small Input";
   part_2 small_input;
   print_endline "Big Input";
   part_2 big_input;  *)
     
   
