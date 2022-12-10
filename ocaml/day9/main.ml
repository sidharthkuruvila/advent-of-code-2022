open Core

let small_input = {|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2|}

                
module Location = struct              
  type t = (int * int) [@@deriving sexp, compare]
  let [@warning "-32"] display_list l =
    let s = List.map l ~f:(fun (x, y) -> Printf.sprintf "(%d, %d)" x y) |> String.concat ~sep:"; " in
    Printf.printf "(%s)\n" s
    
  let [@warning "-32"] to_string (x, y) =
    Printf.sprintf "(%d, %d)" x y


  let equals (a, b) (x, y) =
    a = x && b = y
end

module Location_set = Set.Make(Location)
                
let big_input = In_channel.read_all "day9/input.txt" |> (String.strip ~drop:Char.(fun c -> c = '\n'))


let parse s = String.split s ~on:'\n' |> List.map ~f:(fun line -> let [@warning "-8"] [command; distance] = String.split line ~on:' ' in (command, int_of_string distance))

let expand lines =
  List.concat_map lines ~f:(fun (command, distance) -> List.range 0 distance |> List.map ~f:(fun _ -> command))


let move_1d (tx, ty) =
  if ty = 1 then
    (tx, 0)
  else if tx <> 0 && ty = -1 then
    (0, -1)
  else if tx = 0 then
    (0, -1)
  else (*if ty = 0 then*)
    (tx, -1)

let mul (a, b) (x, y) = (a*x, b*y)
  
let add (a, b) (x, y) = (a+x, b+y)

let switch (a, b) = (b, a)
  
let move direction (tx, ty) =
  match direction with
  | "U" -> ((0, 1), move_1d (tx, ty))
  | "D" -> ((0, -1), move_1d (tx, -1*ty) |> mul (1, -1))
  | "L" -> ((-1, 0), (move_1d (ty, -1*tx)) |> mul (1, -1) |> switch)
  | "R" -> ((1, 0), (move_1d (ty, tx)) |> switch)
  | _ -> failwith "Should never come here"

let step (h_location, t_position, visited) direction =
   let (dp, updated_t_position) = move direction t_position in
      let updated_h_location = add dp h_location in
      (updated_h_location, updated_t_position, (add updated_h_location updated_t_position)::visited)
       
let test_1 s =
  let lines = parse s in
  let commands = expand lines in
  let (_, _, visited) = List.fold_left commands ~init:((0, 0), (0, 0), []) ~f:step in
  visited |> Location_set.of_list |> Location_set.length


let command_to_diff command =
  match command with
  | "U" -> (0, 1)
  | "D" -> (0, -1)
  | "L" -> (-1, 0)
  | "R" -> (1, 0)
  | _ -> failwith "Should not come here"
  


(* cos 0 = 1, sin 0 = 0 x = cost - sint*)
let rotate (cost, sint) (x, y) = (x * cost - y * sint, x * sint + y * cost)

(* let rot0 = (1, 0) *)
let rot90 = (0, 1)
let rot180 = (-1, 0)
let rot270 = (0, -1)
       
let move2_1d dx (tx, ty) =
  if dx = 0 then
    (* (dx, dy) = (1, 0) *)
    move_1d (tx, ty)
  else begin
    assert(dx = 1);
    (* (dx, dy) = (1, 1) *)
    if tx = -1 && ty = -1 then
      (-1, -1)
    else if tx = -1 then (* (-1, 0) , (-1, 1) *)
      (-1, 0)
    else if (tx = 0 && ty = 1) || (tx = 1 && ty = 0) then (* (1, 0) && (0, 1) *)
      (0, 0)
    else if tx = 0 && ty = 0 then (* (0, 0), (1, 1) *)
      (-1, -1)
    else if tx = 1 && ty = 1 then
      (0, 0)
    else if ty = -1 then  (* (0, -1) , (1, -1) *)
      (0, -1)
    else
      failwith "No more options"
    end
        (*
 (-1,  1) (0,  1) (1,  1) 
 (-1,  0) (0,  0) (1,  0) 
 (-1, -1) (0, -1) (1, -1)
         *)

let is_adjacent (a, b) (x, y) =
  abs (a - x) <=1 && abs (b - y) <= 1
  
let move_2 d t =
  let (dx, dy) = d in
  let apply in_rot out_rot d t =
    let (ndx, ndy) = rotate in_rot d in
    assert (ndy = 1);
    let nt = rotate in_rot t in
    let ut = move2_1d ndx nt in
    rotate out_rot ut in
  let open Location in
  if equals d (0, 0) then
    t
  else if equals d (0, 1) || equals d (1, 1) then (* RT *)
    move2_1d dx t
  else if equals d (-1, 0) || equals d (-1, 1) then (* LT *)
    apply rot270 rot90 (dx, dy) t
  else if equals d (-1, -1) || equals d (0, -1) then (* LB *)
     apply rot180 rot180 (dx, dy) t
  else if equals d (1, -1) || equals d (1, 0) then
    apply rot90 rot270 (dx, dy) t (* RB *)
  else begin
      failwith "Should  not come here"
    end


let [@warning "-32"] draw_board head_position rope =
  let rope_positions = List.fold_left rope ~init:[head_position]
                         ~f:(fun [@warning "-8"] (head_position::rest) t ->
                           let knot_position = add t head_position in
                           (knot_position::head_position::rest)) in
  let min_f l ~f =  List.map l ~f:f |> List.min_elt ~compare:Int.compare |> Option.value_exn in
  let max_f l ~f =  List.map l ~f:f |> List.max_elt ~compare:Int.compare |> Option.value_exn in
  let min_x = min_f rope_positions ~f:fst in
  let min_y = min_f rope_positions ~f:snd in
  let max_x = max_f rope_positions ~f:fst in
  let max_y = max_f rope_positions ~f:snd in
  let board_width = max_x -  min_x + 3 in
  let board_height = max_y - min_y + 3 in
  let board = Array.init board_height ~f:(fun _ -> Bytes.init board_width ~f:(fun _ -> '.')) in
  List.iteri rope_positions ~f:(fun i (x, y) ->
      let c = if i = 9 then 'H' else char_of_int (48 + (9 - i)) in
      let board_x = x - min_x + 1 in
      let board_y = max_y - y + 1 in
      Bytes.set(Array.get board board_y) board_x c);
  print_endline "\nDrawing board\n================";
  Array.iter board ~f:(fun row ->
      print_endline (Bytes.to_string row))
  
let update_rope (previous_head_position, updated_head_position, reverse_rope) t =
  let sub a b = add a (mul b (-1, -1)) in
  let d = sub updated_head_position previous_head_position in
  let previous_position = add previous_head_position t in
  if is_adjacent previous_position updated_head_position then
    (previous_position,  previous_position, (sub previous_position  updated_head_position)::reverse_rope)
  else
    let nt = move_2 d t in
    let updated_position = add updated_head_position nt in
    (previous_position, updated_position, nt::reverse_rope)
  
let step_2 (head_position, rope, visited) diff =
  let updated_head_position = add diff head_position in
  let (_, tail_position, reverse_rope) = List.fold_left rope ~init:(head_position, updated_head_position, []) ~f:update_rope in
  (updated_head_position, List.rev reverse_rope, tail_position::visited)
       
let test_2 s =
  let lines = parse s in
  let commands = expand lines in
  let diffs = List.map commands ~f:command_to_diff in
  let rope = List.range 0 9 |> List.map ~f:(fun _ -> (0, 0)) in (* excluding head *)
  List.fold_left diffs ~init:((0, 0), rope, []) ~f:step_2 |> Tuple3.get3 |>  Location_set.of_list |> Location_set.length
            
            
let part_1 s =
  let res = test_1 s in
  Printf.printf "Res: %d\n" res
        
let part_2 s =
  let res = test_2 s in
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
