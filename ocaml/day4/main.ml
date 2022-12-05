open Core

let small_input = {|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8|}

let big_input = In_channel.read_all "day4/input.txt" |> String.strip

let re = Re.Perl.compile_pat "(\\d+)-(\\d+),(\\d+)-(\\d+)"
              
let parse s =
  String.strip s
  |> String.split ~on:'\n'
  |> List.map ~f:(fun x ->
         let res = Re.exec re x in
         let rg n = Re.Group.get res n |> int_of_string in
         (rg 1 , rg 2, rg 3, rg 4))


let overlaps (a1, b1, a2, b2) =
  if a1 <= a2 && b1 >= b2 then
    1
  else if a2 <= a1 && b2 >= b1 then
    1
  else
    0
  
let part_1 s =
  let l = parse s in
  let res = List.map l ~f:overlaps |> List.fold ~init:0 ~f:(+) in
  Printf.printf "Res: %d\n" res
  
let overlaps2 (a1, b1, a2, b2) =
  if a1 <= a2 && a2 <= b1 then
    1
  else if a1 <= b2 && b2 <= b1 then
    1
  else
    overlaps (a1, b1, a2, b2)
  
let part_2 s =
  let l = parse s in
  let res = List.map l ~f:overlaps2 |> List.fold ~init:0 ~f:(+) in
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
     
   
