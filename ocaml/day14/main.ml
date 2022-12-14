open Core

module Point = struct
  type t = (int * int) [@@deriving sexp, compare]
end
             
module Point_set = Set.Make(Point)
                
let small_input = {|498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9|}

let path_split_re = Re.Perl.compile_pat " -> "
                
let parse s =
  let lines = String.split s ~on:'\n' in
  let parse_position ps =
    let [@warning "-8"] [start; stop] = String.split ps ~on:','
                            |> List.map ~f:int_of_string in
    (start, stop) in
  let parse_line line =
    let ps = Re.split path_split_re line in
    let positions = List.map ~f:parse_position ps in
    List.zip_with_remainder positions (List.tl_exn positions) |> fst in
  List.concat_map lines ~f:parse_line


  
let fill_path (ax, ay) (bx, by) =
  let r a b =
    let start = min a b in
    let stop = max a b in
    List.range start (stop + 1) in  
  if ax - bx = 0 then
    r ay by
    |> List.map ~f:(fun d -> (ax, d))
  else if ay - by = 0 then
    r ax bx
    |> List.map ~f:(fun d -> (d, ay))
  else
    failwith "The path is not in a valid direction"
                         
  
let test_1 paths =
  let filled_points_list = List.concat_map paths ~f:(fun (a, b) -> fill_path a b) in
  let filled_points = Point_set.of_list filled_points_list in
  let lowest_point =  List.map filled_points_list ~f:(fun (_, y) -> y)
                      |> List.sort ~compare:(fun a b -> b - a)
                      |> List.hd_exn in
  let is_occupied filled_points (x, y) =
    Point_set.mem filled_points (x, y) in 
  let rec search (x, y) filled_points path count =
    if y > lowest_point then
      count
    else
      if not (is_occupied filled_points (x, y+1)) then
        search (x, y+1) filled_points ((x, y)::path) count
      else if not (is_occupied filled_points (x - 1, y+1)) then
        search (x - 1, y+1) filled_points ((x, y)::path) count
      else if not (is_occupied filled_points (x - 1, y + 1)) then
        search (x - 1, y + 1) filled_points ((x, y)::path) count
      else if not (is_occupied filled_points (x + 1, y + 1)) then
        search (x + 1, y + 1) filled_points ((x, y)::path) count
      else
        let [@warning "-8"] (px, py)::rest = path in
        search (px, py) (Point_set.add filled_points (x, y)) rest (count +1) in
  let count = search (500, 0) filled_points [] 0 in
  count
        

let test_2 paths =
  let filled_points_list = List.concat_map paths ~f:(fun (a, b) -> fill_path a b) in
  let filled_points = Point_set.of_list filled_points_list in
  let lowest_point =  List.map filled_points_list ~f:(fun (_, y) -> y)
                      |> List.sort ~compare:(fun a b -> b - a)
                      |> List.hd_exn in
  let is_occupied filled_points (x, y) =
    Point_set.mem filled_points (x, y) || y > lowest_point + 1 in 
  let rec search (x, y) filled_points path count =
    if not (is_occupied filled_points (x, y+1)) then
      search (x, y+1) filled_points ((x, y)::path) count
    else if not (is_occupied filled_points (x - 1, y+1)) then
      search (x - 1, y+1) filled_points ((x, y)::path) count
    else if not (is_occupied filled_points (x - 1, y + 1)) then
      search (x - 1, y + 1) filled_points ((x, y)::path) count
    else if not (is_occupied filled_points (x + 1, y + 1)) then
      search (x + 1, y + 1) filled_points ((x, y)::path) count
    else
      match path with
      | (px, py)::rest ->
         search (px, py) (Point_set.add filled_points (x, y)) rest (count +1)
      | _ -> count in
  let count = search (500, 0) filled_points [] 0 in
  count + 1
  
let part_1 s =
  let paths = parse s in
  let res = test_1 paths in
  Printf.printf "Res: %d\n" res

let part_2 s =
  let paths = parse s in
  let res = test_2 paths in
  Printf.printf "Res: %d\n" res
                
let big_input = In_channel.read_all "day14/input.txt"
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
