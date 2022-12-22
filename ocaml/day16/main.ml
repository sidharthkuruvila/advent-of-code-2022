open Core
                       
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

let shortest_path paths a b =
  let rec search depth frontier visited =
    if List.mem frontier b ~equal:String.equal then
      Some depth
    else if List.length frontier = 0 then
      None
    else
      let visited = String.Set.union visited (String.Set.of_list frontier) in
      let frontier =
        List.concat_map frontier ~f:(fun c ->  String.Map.find_multi paths c)
        |> List.filter ~f:(fun x -> not (String.Set.mem visited x)) in
      search (depth + 1) frontier visited in
  search 0 [a] String.Set.empty
      
         
let shortest_paths paths rates =
  let valves = "AA" :: (String.Map.keys paths |> List.filter ~f:(fun v -> String.Map.find_exn rates v > 0)) in
  let pairs = List.cartesian_product valves valves
              |> List.filter_map ~f:String.(fun (a, b) -> if a < b then Some (a, b) else None) in
  List.concat_map pairs ~f:(fun (a, b) -> shortest_path paths a b
                                          |> Option.to_list
                                          |>  List.concat_map ~f:(fun d -> [(a, (b, d)); (b, (a, d))]))
  |> String.Map.of_alist_multi

type actor_state = {
    idx: int;
    rate: int;
    total_volume: int;
    minutes_remaining: int;
    current_room: string;
  }
  
type cave_state = {

    actors: actor_state list;
    closed_valves: String.Set.t;
    trace: string list;
  }

let add_trace rates state message =
  let { closed_valves; actors; trace; _} = state in
  let original_open_valves =  String.Map.filter rates  ~f:(fun r -> r > 0) |> String.Map.keys |> String.Set.of_list in
  let open_valves = String.Set.diff  original_open_valves closed_valves |> String.Set.to_list |> String.concat ~sep:", " in
  let actor_traces = List.map actors ~f:(fun {idx; rate; minutes_remaining; current_room; _} ->
                         Printf.sprintf "Actor %d: Rate: %d, Minutes remaining: %d, Current room: %s" idx rate minutes_remaining current_room)
                     |> String.concat ~sep:"\n" in
  let total_volume = List.sum (module Int) actors ~f:(fun {total_volume; _} -> total_volume) in
  let msg = Printf.sprintf {|==  ==
Valves %s are open, total %d pressure released.
%s
%s|} (open_valves) (total_volume) actor_traces message in
  {state with trace = msg::trace}
  
let rec walk_paths_with_distance paths rates state =
  let { closed_valves; actors; _} = state in
  let  [@warning "-8"] actor::rest_actors = List.sort actors ~compare:(fun a1 a2 -> Int.compare a2.minutes_remaining a1.minutes_remaining) in
  let { minutes_remaining; rate; total_volume; current_room; _} = actor in
  if minutes_remaining = 0 then begin
    assert (List.for_all actors ~f:(fun { minutes_remaining; _ } -> minutes_remaining = 0));
    let state = add_trace rates state "Ending early - Done processing" in
    [ List.sum (module Int) actors ~f:(fun {total_volume; _} -> total_volume), state.trace]
  end else if String.Set.length closed_valves = 0 then
    let state = add_trace rates state "All valves opened" in
    [ (List.sum (module Int) actors ~f:(fun {total_volume; rate; minutes_remaining; _} -> total_volume + rate * minutes_remaining)), state.trace]
  else
    String.Map.find_multi paths current_room
    |> List.filter ~f:(fun (room, _) -> String.Set.mem closed_valves room)
    |> List.concat_map ~f:(fun (new_current_room, distance) ->
           let available_distance = if distance < minutes_remaining then distance else minutes_remaining - 1 in
           let state = add_trace rates state (Printf.sprintf "Moving from %s to %s and opening valve" current_room new_current_room) in
           let new_rate = rate + String.Map.find_exn rates new_current_room in
           let total_volume = total_volume + rate * (available_distance + 1) in
           (*assert (not (String.Set.mem closed_valves current_room)); *)
           let closed_valves = String.Set.remove closed_valves new_current_room in
           let actor = {actor with minutes_remaining = minutes_remaining - available_distance - 1; total_volume;  rate = new_rate; current_room = new_current_room  } in
           let state = {state with closed_valves; actors = actor::rest_actors} in
           if available_distance < distance then
             let state = add_trace rates state "No more time" in
             [List.sum (module Int) state.actors ~f:(fun {total_volume; _} -> total_volume), state.trace]
           else
             walk_paths_with_distance paths rates state
             
         )
              
let line_re = Re.Perl.compile_pat "Valve ([A-Z]+) has flow rate=([0-9]+); tunnel(s?) lead(s?) to valve(s?) ([A-Z ,]+)"
let to_valves_split_re = Re.Perl.compile_pat ", "
let parse s =
  String.split s ~on:'\n'
  |> List.map ~f:(fun line ->
         let g = Re.exec line_re line |> Re.Group.all |> Array.to_list in
         let [@warning "-8"] [_; valve; rate_string; _; _; _; to_valves_string] = g in
         (valve, int_of_string rate_string, Re.split to_valves_split_re to_valves_string))

let part_1_fast s =
  let l = parse s in
  let paths = List.map l ~f:(fun (v, _, ns) -> (v, ns)) |> String.Map.of_alist_exn in
  let rates = List.map l ~f:(fun (v, d, _) -> (v, d)) |> String.Map.of_alist_exn in
  let paths_with_distance = shortest_paths paths rates in
  let closed_valves = String.Map.filter rates  ~f:(fun r -> r > 0) |> String.Map.keys |> String.Set.of_list in
  let actor_1 = {idx = 1; total_volume = 0; rate = 0; minutes_remaining = 30; current_room = "AA"; } in
  let start = { closed_valves; trace = []; actors = [actor_1]} in
  let (res, trace) = walk_paths_with_distance paths_with_distance rates start
                     |> List.max_elt ~compare:(fun (a, _) (b, _) -> Int.compare a b) |> Option.value_exn in
  print_endline (String.concat ~sep:"\n\n" (List.rev trace));
  Printf.printf "Res: %d\n" res
  
let [@warning "-32"] big_input = In_channel.read_all "day16/input.txt"
                |> (String.strip ~drop:Char.(fun c -> c = '\n'))


let [@warning "-32"] part_2_fast s =
  let l = parse s in
  let paths = List.map l ~f:(fun (v, _, ns) -> (v, ns)) |> String.Map.of_alist_exn in
  let rates = List.map l ~f:(fun (v, d, _) -> (v, d)) |> String.Map.of_alist_exn in
  let paths_with_distance = shortest_paths paths rates in
  let closed_valves = String.Map.filter rates  ~f:(fun r -> r > 0) |> String.Map.keys |> String.Set.of_list in
  let actor_1 = {idx = 1; total_volume = 0; rate = 0; minutes_remaining = 26; current_room = "AA"; } in
  let actor_2 = {idx = 2; total_volume = 0; rate = 0; minutes_remaining = 26; current_room = "AA"; } in
  let start = { closed_valves; trace = []; actors = [actor_1; actor_2]} in
  let (res, trace) = walk_paths_with_distance paths_with_distance rates start
                     |> List.max_elt ~compare:(fun (a, _) (b, _) -> Int.compare a b) |> Option.value_exn in
  print_endline (String.concat ~sep:"\n\n" (List.rev trace));
  Printf.printf "Res: %d\n" res
  
let [@warning "-32"] big_input = In_channel.read_all "day16/input.txt"
                |> (String.strip ~drop:Char.(fun c -> c = '\n'))
                               
 let _ =
   print_endline "Part 1:";
   print_endline "Small Input";
   part_1_fast small_input;
   print_endline "Big Input";
   part_1_fast big_input;
   print_endline "Part 2:";
   print_endline "Small Input";
   part_2_fast small_input;
   print_endline "Big Input";
   part_2_fast big_input;

