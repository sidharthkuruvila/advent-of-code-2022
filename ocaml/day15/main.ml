open Core

module Point = struct
  type t = (int * int) [@@deriving sexp, compare]
end
             
module Point_set = Set.Make(Point)
                
let small_input = {|Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3|}

let line_re = Re.Perl.compile_pat "Sensor at x=(-?(?:[0-9]+)), y=(-?(?:[0-9]+)): closest beacon is at x=(-?(?:[0-9]+)), y=(-?(?:[0-9]+))"

let parse s =
  String.split s ~on:'\n'
  |> List.map ~f:(fun line ->
         let g = Re.exec line_re line |> Re.Group.all |> Array.to_list |> List.tl_exn |> List.map ~f:int_of_string in
         let [@warning "-8"] [sx; sy; bx; by] = g in
         ((sx, sy), (bx, by)))
                      
let manhattan_distance (x1, y1) (x2, y2)  = (abs (x1 - x2)) + (abs (y1 - y2))
  
let test_1 l row =
  let beacons_on_row = List.filter_map l ~f:(fun (_, (x, y)) -> if y = row then Some x else None) |> Int.Set.of_list in
  let sensor_ranges =
    List.concat_map l ~f:(fun (s, b) ->
        let (x, _) = s in
        let rd = manhattan_distance s (x, row) in
        let bd = manhattan_distance s b in
       if rd < bd then
          let w = bd - rd in
          [(x - w, x + w)]
        else
          []
      )
    |> List.sort ~compare:Point.compare in
  let rec loop sensor_ranges n count =
    match sensor_ranges with
    | (s, e)::rest ->
       assert (s < e);
       let f = Int.max n s in
       if f < e then
         let beacons = List.range f (e + 1) |> List.count ~f:(Int.Set.mem beacons_on_row) in
         loop rest (e + 1) (count + (e - f) + 1 - beacons)
       else
         loop rest n count
    | [] -> count in
  let count = loop sensor_ranges Int.min_value 0 in
  count

let test_2 l row =
  let beacons_on_row = List.filter_map l ~f:(fun (_, (x, y)) -> if y = row then Some x else None) |> Int.Set.of_list in
  let sensor_ranges =
    List.concat_map l ~f:(fun (s, b) ->
        let (x, _) = s in
        let rd = manhattan_distance s (x, row) in
        let bd = manhattan_distance s b in
       if rd < bd then
          let w = bd - rd in
          [(x - w, x + w)]
        else
          []
      )
    |> List.sort ~compare:Point.compare in
  let rec loop sensor_ranges n =
    match sensor_ranges with
    | (s, e)::rest ->
       assert (s < e);
       if n+1 < s && not (Int.Set.mem beacons_on_row (n+1)) then
         Some ((n+1), row)
       else
         let f = Int.max n s in
         if f < e then
           loop rest e
         else
           loop rest n
    | [] -> None in
  if List.length sensor_ranges = 0 then
    None
  else
    loop sensor_ranges (List.hd_exn sensor_ranges |> fst)
  
let part_1 s n =
  let l = parse s in
  let res = test_1 l n in
  Printf.printf "Res: %d\n" res

let part_2 s n =
  let l = parse s in
  let candidates: (int * int) list = List.range 0 (n + 1) |> List.filter_map ~f:(test_2 l) in
  assert (List.length candidates = 1);
  let (x, y) = List.hd_exn candidates in
  let res = x*4000000 + y in
  Printf.printf "Res: %d\n" res
  
let big_input = In_channel.read_all "day15/input.txt"
                |> (String.strip ~drop:Char.(fun c -> c = '\n'))

  
 let _ =
   print_endline "Part 1:";
   print_endline "Small Input";
   part_1 small_input 10;
   print_endline "Big Input";
   part_1 big_input 2000000;
   print_endline "Part 2:";
   print_endline "Small Input";
   part_2 small_input 20;
   print_endline "Big Input";
   part_2 big_input 4000000;


   (*
5688620
5688617
    *)
