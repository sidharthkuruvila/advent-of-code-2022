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
  String.fold s ~init:0 ~f:(fun acc c -> (1 lsl (priority c - 1)) lor acc)

let rec one_pos n =
  if n = 0 then 0
  else 1 + one_pos (n lsr 1)
  
let find l =
  List.groupi l ~break:(fun i _ _ -> (i mod 3) = 0)
  |> List.map ~f:(fun [@warning "-8"] [a; b; c] -> one_pos (a land b land c))
  |> List.fold ~init:0 ~f:(+)
            
let part_2 input =
  let l = parse_part_2 input in
  let ints = List.map l ~f:sack_to_int in
  let res = find ints in
  Printf.printf "Res: %d\n" res
          
  
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
   part_2 big_input
     
   
