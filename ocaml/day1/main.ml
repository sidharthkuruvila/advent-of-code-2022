let rec loop l acc max =
  match l with
  | ""::rest -> loop rest 0 (if acc > max then acc else max)
  | [] -> max
  | sn::rest -> loop rest (acc + int_of_string sn) max
              
              
let top3 l = 
  let rec loop l acc =
    match l with
    | ""::rest -> acc :: loop rest 0
    | [] -> [acc]
    | sn::rest -> loop rest (acc + int_of_string sn) in
  let sums = loop l 0 in
  let sorted = List.sort (fun a b -> (-1) * Int.compare a b) sums in
  List.map (List.nth sorted) [0;1;2] |> List.fold_left (+) 0
  

let parse s =
  String.split_on_char '\n' s
  

let small_input = {|1000
2000
3000

4000

5000
6000

7000
8000
9000

10000|} 

let part_1 input =
  let l = parse input in
  let res = loop l 0 0 in
  Printf.printf "Res: %d\n" res
   
let part_2 input =
  let l = parse input in
  let res = top3 l in
  Printf.printf "Res: %d\n" res
                 
 let big_input = let ch = open_in "day1/input.txt" in really_input_string ch (in_channel_length ch)
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
     
   
