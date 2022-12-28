open Core
                       
let [@warning "-32"] small_input = {|>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>|}

module Point = struct
  type t = (int * int) [@@deriving sexp, compare]
end

module Point_set = Set.Make(Point)
             
let shapes =
  [
    "####";
    ".#.\n###\n.#.";
    "..#\n..#\n###";
    "#\n#\n#\n#";
    "##\n##";
  ]
  |> List.map ~f:(fun s ->
         let lines = String.split s ~on:'\n' in
         let h = List.length lines in
         List.concat_mapi lines ~f:(fun i line ->
             let l = String.to_list line in
             List.filter_mapi l ~f:Char.(fun j ch -> if ch = '.' then None else Some (j + 2, h - i - 1))))
  |> List.to_array

let can_move_to occupied shape =
  List.for_all shape ~f:(fun (x, y) -> 0 <= x && x < 7 && y>=0 &&  not (Point_set.mem occupied (x, y)))
           
let [@warning "-32"] big_input =
  In_channel.read_all "day17/input.txt"
  |> (String.strip ~drop:Char.(fun c -> c = '\n'))

(*let add_scalar points (x, y) = List.map points ~f:(fun (px, py) -> (x + px, y + py))*)

let translate points (x, y) = List.map points ~f:(fun (px, py) -> (x + px, y + py))

let [@warning "-32"] print_points l = List.map l ~f:(fun (a, b) -> Printf.sprintf "(%d, %d)" a b) |> String.concat ~sep:", " |> print_endline                          

let try_move occupied shape direction =
  let moved_shape = translate shape direction in
  if can_move_to occupied moved_shape then
    (moved_shape, true)
  else
    (shape, false)

  
let [@warning "-32"] draw occupied shape =
  let max_occupied = Point_set.to_list occupied |> List.map ~f:snd |> List.max_elt ~compare:Int.compare |> Option.value_exn in
  let max = List.map shape ~f:snd |> List.max_elt ~compare:Int.compare |> Option.value ~default:max_occupied in
  Printf.printf "Max: %d\n" max;
  let shape = Point_set.of_list shape in
  List.range ~stride:(-1) ~stop:`inclusive max 0
  |> List.iter ~f:(fun y ->
         print_string "|";
         List.range 0 7
         |> List.iter ~f:(fun x ->
                if Point_set.mem occupied (x, y) then
                  print_string "#"
                  else if Point_set.mem shape (x, y) then
                  print_string "@"
                  else
                    print_string "."
              );
         print_endline "|");
  print_endline "+-------+"



let try_snapshot (*~command_index ~commands*) ~shape_index ~occupied ~highest snapshots =
  if (*command_index mod String.length commands = 0
                         && *) shape_index mod Array.length shapes = 0 && shape_index > 0 then
    let (snapshot_list, last_occupied, last_highest, last_shape_index) = snapshots in
    let snapshot_occupied = Point_set.diff occupied last_occupied |> Point_set.map ~f:(fun (x, y) -> (x, y - last_highest)) in
    let snapshot_height = highest - last_highest in
    let snapshot_shape_count = shape_index - last_shape_index in
    let snapshot_list = (snapshot_occupied, snapshot_height, snapshot_shape_count)::snapshot_list in
    (snapshot_list, occupied, highest, shape_index)
  else
    snapshots

let find_repeats snapshots =
  let (snapshot_list, _, _, _) = snapshots in
  let len =  List.length snapshot_list in
(*  Printf.printf "Find repeats len: %d\n" len;*)
  if len > 100 && len mod 2 = 0 then
    let sublist = List.take snapshot_list ((len - 100)) in
    (*    Printf.printf "Find repeats\n======";*)
    let (a, b) = List.split_n sublist ((len - 100)/2) in
    let (occupieds_a, heights_a, shape_counts_a) = List.unzip3 a in
    let (occupieds_b, heights_b, shape_counts_b) = List.unzip3 b in
    if List.equal Point_set.equal occupieds_a occupieds_b then
      
      let total_height_a = List.sum (module Int) heights_a ~f:Fn.id in
      let total_height_b = List.sum (module Int) heights_b ~f:Fn.id in
      let total_shape_count_a = List.sum (module Int) shape_counts_a ~f:Fn.id in
      let total_shape_count_b = List.sum (module Int) shape_counts_b ~f:Fn.id in
      Printf.printf "total height: %d, total_shape_count: %d\n"  total_height_a total_shape_count_b;
      assert (total_shape_count_a = total_shape_count_b);
      assert (total_height_a = total_height_b);
      Some (total_height_a, total_shape_count_a)
    else 
      None
  else
    None

let drop_shape ~commands ~occupied ~highest ~shape_index ~command_index =
  let new_shape = Array.get shapes (shape_index mod (Array.length shapes)) in
  let shape = translate new_shape (0, highest + 4) in
  let rec loop shape command_index =
    let command = String.get commands (command_index mod (String.length commands)) in
    let direction = match command with
      | '<' -> -1
      | '>' -> 1
      | _ -> failwith "Unexpected command" in
     let (shape, _) = try_move occupied shape (direction, 0) in
     let (shape, moved) = try_move occupied shape (0, -1) in
     if moved then
       loop shape (command_index + 1)
     else
       let occupied = List.fold_left shape ~init:occupied ~f:(fun occupied point -> Point_set.add occupied point) in
       let new_highest = List.map shape  ~f:snd |> List.max_elt ~compare:Int.compare |> Option.value_exn in
       let highest = Int.max new_highest highest in
       (occupied, highest, shape_index + 1, command_index + 1) in
  loop shape command_index

let snapshot_strategy  ~shape_index ~occupied ~highest snapshots =
  let snapshots = try_snapshot (*~command_index ~commands*) ~shape_index ~occupied ~highest snapshots in
  (snapshots, find_repeats snapshots)

let [@warning "-27"] no_snapshots ~shape_index ~occupied ~highest snapshots =
  (snapshots, None)
  
let run commands total_shapes ~snapshot_strategy =
  let empty_snapshot =  ([], Point_set.empty, 0, 0) in
  let rec loop total_shapes occupied highest shape_index command_index snapshots snapshot_strategy  =
    if shape_index = total_shapes then
        highest
    else                                  
      let (occupied, highest, shape_index, command_index) =
        drop_shape ~occupied ~highest ~shape_index ~commands ~command_index in
      let (snapshots, repeats) = snapshot_strategy  ~shape_index ~occupied ~highest snapshots in
      match repeats with
      | Some (height, shape_count) ->
         let remaining_shape_count = total_shapes - shape_index in
         let repeat_count = remaining_shape_count / shape_count in
         let repeat_height = height * repeat_count in
         let rem = remaining_shape_count mod shape_count in
         assert (shape_index mod 5 = 0);
         Printf.printf "Shape index: %d, rmaininng shape count: %d\n" shape_index remaining_shape_count;
         Printf.printf "Remaining shapes: %d command index mod: %d\n " rem (command_index mod String.length commands);
         let rem_highest = loop (shape_index + rem ) occupied highest (shape_index) 0 empty_snapshot no_snapshots - highest in
         Printf.printf "Base height: %d, repeated height: %d, remaining height: %d\n" highest repeat_height rem_highest;
         (highest + repeat_height + rem_highest)
      | None -> loop total_shapes occupied highest shape_index command_index snapshots snapshot_strategy in
  loop total_shapes Point_set.empty (-1) 0 0 empty_snapshot snapshot_strategy
 
let [@warning "-32"] part_1 s =
  let highest = run s 2022 ~snapshot_strategy in
  Printf.printf "Res: %d\n" (highest + 1);
  let highest = run s 2004 ~snapshot_strategy:no_snapshots in
  Printf.printf "Res: %d\n" (highest + 1)  ;
  let highest = run s 570 ~snapshot_strategy:no_snapshots in
      Printf.printf "Res: %d\n" (highest + 1)
      
let [@warning "-32"] part_2 s =
  let highest = run s 1000000000000 ~snapshot_strategy in
  Printf.printf "Res: %d\n" (highest + 1)
 
let [@warning "-32"] big_input = In_channel.read_all "day17/input.txt"
                |> (String.strip ~drop:Char.(fun c -> c = '\n'))
                               
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

