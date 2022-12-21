open Core


module String_pair = struct
  type t = (string * string)  [@@deriving sexp, compare]
end

module String_pair_set = Set.Make(String_pair)
module String_pair_map = Map.Make(String_pair)
                       
let small_input = {|Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II|}

let line_re = Re.Perl.compile_pat "Valve ([A-Z]+) has flow rate=([0-9]+); tunnel(s?) lead(s?) to valve(s?) ([A-Z ,]+)"
let to_valves_split_re = Re.Perl.compile_pat ", "
let parse s =
  String.split s ~on:'\n'
  |> List.map ~f:(fun line ->
         let g = Re.exec line_re line |> Re.Group.all |> Array.to_list in
         let [@warning "-8"] [_; valve; rate_string; _; _; _; to_valves_string] = g in
         (valve, int_of_string rate_string, Re.split to_valves_split_re to_valves_string))

type cave_state = {
    total_volume: int;
    rate: int;
    seconds_remaining: int;
    current_room: string;
    closed_valves: String.Set.t;
    visited: String.Set.t;
    trace: string list;
  }

let move_room paths state =
  let { current_room; total_volume; rate; seconds_remaining; visited; _ } = state in
  String.Map.find_multi paths current_room
  |> List.filter_map ~f:(fun current_room ->
         if String.Set.mem visited current_room then
           None
         else Some
         {
           state with total_volume = total_volume + rate; rate;
                      seconds_remaining = seconds_remaining - 1;
                      current_room; visited = String.Set.add visited current_room
       })
let do_nothing state =
  let {seconds_remaining; total_volume; rate; _} = state in
  [{state with seconds_remaining = seconds_remaining - 1; total_volume = total_volume + rate }]

let open_valve rates state =
  let { total_volume; rate; current_room; seconds_remaining; closed_valves; _ } = state in
  let additional_rate = String.Map.find_exn rates current_room in
  let new_rate = rate + additional_rate in
  let closed_valves = String.Set.remove closed_valves current_room in
  let seconds_remaining = seconds_remaining - 1 in
  [{state with total_volume = total_volume + rate; rate = new_rate; seconds_remaining; closed_valves; visited = String.Set.empty}]
  
let is_all_valves_opened state =
  let {closed_valves; _} = state in
  String.Set.length closed_valves = 0

let is_can_open_valve state =
  let {closed_valves; current_room; _} = state in
  String.Set.mem closed_valves current_room
  
let time_finished state =
  let {seconds_remaining; _} = state in seconds_remaining = 0
                                      
let get_total_volume state =
  let {total_volume; _} = state in total_volume

let add_trace rates state message =
  let { rate; closed_valves; seconds_remaining; trace; _} = state in
  let original_open_valves =  String.Map.filter rates  ~f:(fun r -> r > 0) |> String.Map.keys |> String.Set.of_list in
  let open_valves = String.Set.diff  original_open_valves closed_valves |> String.Set.to_list |> String.concat ~sep:", " in
  let msg = Printf.sprintf {|== Minute %d ==
Valves %s are open, releasing %d pressure.                 
%s|} (30 - seconds_remaining) (open_valves) (rate) message in
  {state with trace = msg::trace}

let open_valve_trace rates state =
  add_trace rates state (Printf.sprintf "You open valve %s" state.current_room)

let move_room_trace rates state =
  add_trace rates state (Printf.sprintf "You move to valve %s" state.current_room)

let do_nothing_trace rates state =
   add_trace rates state ""
  
let rec walk paths rates state =
  if time_finished state then begin
      [(get_total_volume state, state.trace)] end
  else 
    let nexts = if is_all_valves_opened state then
                  do_nothing state |> List.map ~f:(do_nothing_trace rates)
                else if is_can_open_valve state then
                  List.concat [open_valve rates state |> List.map ~f:(open_valve_trace rates);  move_room paths state |> List.map ~f:(move_room_trace rates)]
                else
                  move_room paths state |> List.map ~f:(move_room_trace rates) in
    List.concat_map nexts ~f:(walk paths rates)
 
  
let part_1 s =
  let l = parse s in
  let paths = List.map l ~f:(fun (v, _, ns) -> (v, ns)) |> String.Map.of_alist_exn in
  let rates = List.map l ~f:(fun (v, d, _) -> (v, d)) |> String.Map.of_alist_exn in
  let closed_valves = String.Map.filter rates  ~f:(fun r -> r > 0) |> String.Map.keys |> String.Set.of_list in
  let start = {total_volume = 0; rate = 0; seconds_remaining = 30; current_room = "AA"; closed_valves; visited = String.Set.empty; trace = [] } in
  let (res, trace) = walk paths rates start  |> List.max_elt ~compare:(fun (a, _) (b, _) -> Int.compare a b) |> Option.value_exn in
  print_endline (String.concat ~sep:"\n\n" trace);
  Printf.printf "Res: %d\n" res

  
let [@warning "-32"] big_input = In_channel.read_all "day15/input.txt"
                |> (String.strip ~drop:Char.(fun c -> c = '\n'))

  
 let _ =
   print_endline "Part 1:";
   print_endline "Small Input";
   part_1 small_input;
   (*print_endline "Big Input";
   part_1 big_input 2000000;
   print_endline "Part 2:";
   print_endline "Small Input";
   part_2 small_input 20;
   print_endline "Big Input";
   part_2 big_input 4000000;*)

