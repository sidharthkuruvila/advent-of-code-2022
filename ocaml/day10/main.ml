open Core


let small_input =  In_channel.read_all "day10/small_input.txt" |> (String.strip ~drop:Char.(fun c -> c = '\n'))
let big_input = In_channel.read_all "day10/input.txt" |> (String.strip ~drop:Char.(fun c -> c = '\n'))

type instr =
  | Noop
  | Addx of int

let [@warning "-32"] instr_to_string instr =
  match instr with
  | Noop -> "noop"
  | Addx n -> Printf.sprintf "addx %d" n

let parse s =
  String.split s ~on:'\n'
  |> List.map ~f:(fun line ->
         if String.(line = "noop") then
           Noop
         else
           let  [@warning "-8"] [_; ns] = String.split line ~on:' ' in
           Addx (int_of_string ns))

let run_cycle  (acc, current_cycle, next_cycles, total_strength) instr =
  let (acc_incr, cycle_incr)  =  match instr with
    | Noop -> (0, 1)
    | Addx n -> (n, 2) in
  let (new_strength, next_cycles) =
    match next_cycles with
    | cycle::rest when cycle <= current_cycle + cycle_incr -> ((instr, Some (cycle, acc)), rest)
    | _ ->  ((instr, None), next_cycles) in
  (acc + acc_incr, current_cycle + cycle_incr, next_cycles, new_strength::total_strength)
                  
  
let test_1 instrs =
  let next_cycles = [20; 60; 100; 140; 180; 220] in
  let (_, _, _, strength_list) = List.fold_left instrs ~init:(1, 0, next_cycles, []) ~f:run_cycle
 in
(* List.iter strength_list ~f:(fun o ->
     match o with
     | (instr, None) -> Printf.printf "%s -> No update\n" (instr_to_string instr)
     | (instr, Some (cycle, signal)) -> Printf.printf "%s -> cycle:%d, signal: %d\n" (instr_to_string instr) cycle signal
   );*)
 List.filter_map strength_list ~f:(fun (_, o) -> o) |>
   List.fold_left ~init:0 ~f:(fun acc (cycle, signal) -> acc + cycle*signal) 
 
let part_1 s =
  let instrs = parse s in
  let res = test_1 instrs in
  Printf.printf "Res: %d\n" res


  
let run_cycle_2 (sprite_position, current_cycle, state) instr =
  let contains sprite_position cycle =
    let cm = cycle mod 40 in
    sprite_position - 1 <= cm && cm <= sprite_position + 1 in
  let make_state ~sprite_position ~cycle =
    (cycle, sprite_position, contains sprite_position cycle) in
  match instr with
  | Noop -> (sprite_position, current_cycle + 1, (make_state ~sprite_position  ~cycle:(current_cycle + 1))::state)
  | Addx n ->
     let updated_sprite_position = sprite_position + n in
     (updated_sprite_position, current_cycle + 2,
      make_state ~cycle:(current_cycle + 2) ~sprite_position:updated_sprite_position
      ::(make_state ~cycle:(current_cycle + 1) ~sprite_position)
      ::state)
   
let part_2 s =
  let instrs = parse s in
  let (_, _, state) = List.fold_left instrs ~init:(1, 0, [(0, 0, true)]) ~f:run_cycle_2 in
  assert(List.length state = 241);
  List.groupi (List.rev state) ~break:(fun i _ _ -> (i mod 40 = 0))
  |> List.iter ~f:(fun row ->
         List.iter row ~f:(fun (_, _, draw_pixel) -> if draw_pixel then Printf.printf "#" else Printf.printf ".");
           print_endline ""
       )
  

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
