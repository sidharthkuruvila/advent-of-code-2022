open Core

let small_input = {|Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi|}

let neighbours = [(1,0);(-1,0);(0,1);(0,-1)]



let add (a, b) (x, y) = (a + x, b + y)

                      
let fewest_steps grid_str =
  let grid = String.split grid_str ~on:'\n' |> List.map ~f:String.to_array |> List.to_array in
  let height = Array.length grid in
  let width = Array.get grid 0 |> Array.length in
  let in_grid (x, y) = 0 <= x && x < width && 0 <= y && y < height in
  let get (x, y) = Array.get (Array.get grid y) x |> int_of_char in
  let positions = List.cartesian_product (List.range 0 width) (List.range 0 height) in
  let idx (x, y) = y*width + x in
  let start = List.find_exn positions ~f:(fun position -> get position = (int_of_char 'S')) |> idx in
  let stop = List.find_exn positions ~f:(fun position -> get position = (int_of_char 'E')) |> idx in
  let steps = List.concat_map positions ~f:(fun position ->
                  List.map neighbours ~f:(add position)
                  |> List.filter ~f:in_grid
                  |> List.map ~f:(fun to_position -> (position, to_position))
                  |> List.filter ~f:(fun (from_position, to_position) ->
                                         let a = get from_position in
                                         let b = get to_position in
                                         let a = if a = (int_of_char 'S') then (int_of_char 'a') else a in
                                         let b = if b = (int_of_char 'E') then (int_of_char 'z') else b in
                                         b - a <= 1)
                  |> List.map ~f:(fun (from_position, to_position) -> (idx from_position , idx to_position))) in
  let steps_map = Int.Map.of_alist_multi steps in
  let rec search distance ~visited ~frontier =
    let nexts  = Int.Set.to_list frontier
    |> List.concat_map ~f:(fun n ->
           Int.Map.find_multi steps_map n
           |> List.filter ~f:(fun n -> not (Int.Set.mem visited n))) 
                 |> Int.Set.of_list in
    assert(not (Int.Set.is_empty nexts));
    if Int.Set.mem nexts stop then
      distance
    else
      let visited = Int.Set.union visited nexts in
      let frontier = nexts in
      search (distance + 1) ~visited ~frontier in
  search 1 ~visited:(Int.Set.of_list [start]) ~frontier:(Int.Set.of_list [start])
  
let fewest_steps_2 grid_str =
  let grid = String.split grid_str ~on:'\n' |> List.map ~f:String.to_array |> List.to_array in
  let height = Array.length grid in
  let width = Array.get grid 0 |> Array.length in
  let in_grid (x, y) = 0 <= x && x < width && 0 <= y && y < height in
  let get (x, y) = Array.get (Array.get grid y) x |> int_of_char in
  let positions = List.cartesian_product (List.range 0 width) (List.range 0 height) in
  let idx (x, y) = y*width + x in
  let stop = List.find_exn positions ~f:(fun position -> get position = (int_of_char 'E')) |> idx in
  let steps = List.concat_map positions ~f:(fun position ->
                  List.map neighbours ~f:(add position)
                  |> List.filter ~f:in_grid
                  |> List.map ~f:(fun to_position -> (position, to_position))
                  |> List.filter ~f:(fun (from_position, to_position) ->
                                         let a = get from_position in
                                         let b = get to_position in
                                         let a = if a = (int_of_char 'S') then (int_of_char 'a') else a in
                                         let b = if b = (int_of_char 'E') then (int_of_char 'z') else b in
                                         b - a <= 1)
                  |> List.map ~f:(fun (from_position, to_position) -> (idx from_position , idx to_position))) in
  let steps_map = Int.Map.of_alist_multi steps in
  let rec search distance ~visited ~frontier =
    let nexts  = Int.Set.to_list frontier
    |> List.concat_map ~f:(fun n ->
           Int.Map.find_multi steps_map n
           |> List.filter ~f:(fun n -> not (Int.Set.mem visited n))) 
                 |> Int.Set.of_list in
    if Int.Set.is_empty nexts then
      Int.max_value
    else if Int.Set.mem nexts stop then
      distance
    else
      let visited = Int.Set.union visited nexts in
      let frontier = nexts in
      search (distance + 1) ~visited ~frontier in
  let starts = List.filter positions ~f:(fun position -> get position = int_of_char 'a') |> List.map ~f:idx in
  List.map starts ~f:(fun start ->  search 1 ~visited:(Int.Set.of_list [start]) ~frontier:(Int.Set.of_list [start]))
  |> List.min_elt ~compare:Int.compare |> Option.value_exn
 
    
let part_1 s =
  let res = fewest_steps s in
  Printf.printf "Res: %d\n" res
                                            
let part_2 s =
  let res = fewest_steps_2 s in
  Printf.printf "Res: %d\n" res
                
let big_input = In_channel.read_all "day12/input.txt" |> (String.strip ~drop:Char.(fun c -> c = '\n'))

  
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
