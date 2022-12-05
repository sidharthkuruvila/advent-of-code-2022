let small_input = {|A Y
B X
C Z|}

let big_input = let ch = open_in "day2/input.txt" in really_input_string ch (in_channel_length ch)

let get_p1_id s =
  match s with
  | "A" -> 1
  | "B" -> 2
  | "C" -> 3
  | _ -> failwith "Unknown input"
         
let get_p2_id s =
  match s with
  | "X" -> 1
  | "Y" -> 2
  | "Z" -> 3
  | _ -> failwith "Unknown input"



let parse s =
  String.trim s
  |> String.split_on_char '\n'
  |> List.map (fun s  ->
         let [@warning "-8"] [a; b] = String.split_on_char ' ' s in (get_p2_id b, get_p1_id a))
  
let score l =
  let calc_score (my_move, their_move) =
    let round_score  = match (my_move, their_move) with
      | (_, _) when their_move = my_move -> 3
      | (1,  2) | (2, 3) | (3, 1) ->  0
      | _ -> 6 in
      round_score + my_move in
  List.map calc_score l  |> List.fold_left (+) 0


let get_id s =
  match s with
  | "A" -> 0
  | "B" -> 1
  | "C" -> 2
  | _ -> failwith "Unknown input"  
  
let parse_part_2 s =
  String.trim s
  |> String.split_on_char '\n'
  |> List.map (fun s  ->
         let [@warning "-8"] [a; b] = String.split_on_char ' ' s in (get_id a, b))
  
let part_1 input =
  let l = parse input in
  let res = score l in
  Printf.printf "Res: %d\n" res


let winning_move their_move = (their_move + 1) mod 3
let losing_move their_move = (their_move + 2) mod 3

let get_my_move their_move result =
  if result = "X" then
    losing_move their_move
  else if result = "Z" then
    winning_move their_move
  else
    their_move

let get_result_score result =
  match result with
  | "X" -> 0
  | "Y" -> 3
  | "Z" -> 6
  | _ -> failwith "Unknown input"
  
let get_move_score move = move + 1  
let score_2 l =
  let calc_score (their_move, result) =
    let my_move = get_my_move their_move result in
    let move_score = get_move_score my_move in
    let result_score = get_result_score result in
    move_score + result_score in
  List.map calc_score l  |> List.fold_left (+) 0
  
                     
let part_2 input =
  let l = parse_part_2 input in
  let res = score_2 l in
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
   part_2 big_input;  
     
   
