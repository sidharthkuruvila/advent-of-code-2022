open Core

let small_input = {|30373
25512
65332
33549
35390|}


  
let big_input = In_channel.read_all "day8/input.txt" |> (String.strip ~drop:Char.(fun c -> c = '\n'))


             
let test_1 s ~width =
  let grid = String.split s ~on:'\n' |> String.concat |> String.to_array |> Array.map ~f:int_of_char in
  let get n = Array.get grid n in 
  let idx (x, y) = width * y + x in
  
  let directions = [( 1,  0, width - 1, 0, -1);
                    ( 1,  0, 0, width - 1, 1);
                    ( 0,  1, width - 1, 0, -1);
                    ( 0,  1, 0, width - 1, 1)] in
  let ray_gun (xm, ym, start, stop, stride) index =
    List.range ~stride start stop ~stop:`inclusive
    |> List.map ~f:(fun a -> idx (index*ym + a*xm, index*xm +  a*ym)) in
  let visibles = List.concat_map directions ~f:(fun d ->
               List.range 0 width
               |> List.concat_map ~f:(fun i ->
                      let ray = ray_gun d i in
                      let res = List.fold_left ray ~init:(0, []) ~f:(fun (min_height, visible) i -> let height = get i in if height > min_height then (height, i::visible) else (min_height, visible)) in
                      snd res
                      
                   )) in
  visibles |> Int.Set.of_list |> Int.Set.length

let test_2 s ~width =
  let grid = String.split s ~on:'\n' |> String.concat |> String.to_array |> Array.map ~f:int_of_char in
  let get n = Array.get grid n in 
  let idx (x, y) = width * y + x in
  let within_range (x, y) = 0 <= x && x < width && 0 <= y && y < width in
  let directions = [(1, 0); (-1, 0); (0, 1); (0, -1)] in
  let score_in_direction (x, y) (dx, dy) =
    let height = get (idx (x, y)) in
    let rec loop x y =
      let (nx, ny) = (x+dx, y+dy) in
      if within_range (nx, ny) then
        if get (idx (nx, ny)) < height then
          1 + (loop nx ny)
        else
          1
      else
        0
    in
    loop x y in
  let score pos =
    List.map directions ~f:(score_in_direction pos)
    |> List.fold_left ~init:1 ~f:( * ) in
  let candidates = List.range 1 (width - 1) |> List.concat_map ~f:(fun x -> List.range 1 (width - 1) |> List.map ~f:(fun y -> (x, y))) in
  List.map candidates ~f:score |> List.max_elt ~compare:Int.compare  |> Option.value_exn
  
      

  
let part_1 s =
  let width = String.split s ~on:'\n' |> List.length in
  let res = test_1 s ~width in
  Printf.printf "Res: %d\n" res
        
let part_2 s =
  let width = String.split s ~on:'\n' |> List.length in
  let res = test_2 s ~width in
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
     
   
