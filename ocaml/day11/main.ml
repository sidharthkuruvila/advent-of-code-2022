open Core


let small_input =  In_channel.read_all "day11/small_input.txt" |> (String.strip ~drop:Char.(fun c -> c = '\n'))
let big_input = In_channel.read_all "day11/input.txt" |> (String.strip ~drop:Char.(fun c -> c = '\n'))

let re_str = {|Monkey (\d+):
  Starting items: ([\d, ]+)
  Operation: new = old ([+*]) (old|\d+)
  Test: divisible by (\d+)
    If true: throw to monkey (\d+)
    If false: throw to monkey (\d+)|}

let re = Re.Perl.compile_pat re_str

type op =
  | Add of int
  | Mul of int
  | Square
       
type monkey = {
    monkey_id: int;
    starting_items: int list;
    operation: op;
    divisibility: int;
    if_true: int;
    if_false: int;
  }


let split_re = Re.Perl.compile_pat "\n\n"
let parse s =
  Re.split split_re s
  |> List.map ~f:(fun monkey_str ->
         let m = Re.exec re monkey_str in
         let arr = Re.Group.all m in
         let [@warning "-8"] [| _;
                monkey_id_str;
                starting_items_str;
                operator_str;
                operation_value_str;
                divisibility_str;
                if_true_str;
                if_false_str;
               |] = arr in
         let monkey_id = int_of_string monkey_id_str in
         let starting_items = String.split starting_items_str ~on:',' |> List.map ~f:(fun s -> String.strip s |> int_of_string) in
         let operation = match operation_value_str with
           | "old" -> Square
           | _ ->
              let n = int_of_string operation_value_str in
              match operator_str with
              | "+" -> Add n
              | "*" -> Mul n
              | _ -> failwith "Unkown operation" in
         let divisibility = int_of_string divisibility_str in
         let if_true = int_of_string if_true_str in
         let if_false = int_of_string if_false_str in
         {
           monkey_id;
           starting_items;
           operation;
           divisibility;
           if_true;
           if_false;
         }
       )

let get_start_list l = List.concat_map l ~f:(fun m -> List.map m.starting_items ~f:(fun worry_level -> (m.monkey_id, worry_level)))
  

let part_1 s =
  let monkeys = parse s in
  let worry_list = get_start_list monkeys in
  let worry_counts worry_list =
    List.map worry_list ~f:(fun (monkey_id, _) -> (monkey_id, ()) ) |> Int.Map.of_alist_fold ~init:0 ~f:(fun a _ -> a + 1 ) in
  let apply_operation operation worry =
    match operation with
    | Add n -> worry + n
    | Mul n -> worry * n
    | Square -> worry * worry in
  let monkey_moves { monkey_id; operation; divisibility; if_true; if_false; _ } worry_list =
    let (current_worries, rest) =  List.partition_tf worry_list  ~f:(fun (i, _) -> i = monkey_id) in
    
    let updated_worries =
      List.map current_worries ~f:(fun (_, worry) ->
          let inspected_worry = apply_operation operation worry in
          let shrunken_worry = inspected_worry / 3 in
          if shrunken_worry mod divisibility = 0 then
            (if_true, shrunken_worry)
          else
            (if_false, shrunken_worry)
        ) in
    (List.concat [rest; updated_worries],
     worry_counts current_worries) in
  let rounds = List.range 0 20 |> List.map ~f:(fun _ -> monkeys) in
  let run_round monkeys worry_list =
    List.fold_left monkeys ~init:(worry_list, []) ~f:(fun (worry_list, worry_counts) monkey ->
      let (updated_worry_list, additional_worry_counts) = monkey_moves monkey worry_list in
      (updated_worry_list, additional_worry_counts::worry_counts)) in
  let (_, final_worry_counts) =
    List.fold_left rounds ~init:(worry_list, []) ~f:(fun (worry_list, worry_counts) monkeys ->
        let (updated_worry_list, additional_worry_counts) = run_round monkeys worry_list in
        (*Printf.printf "\nRound:\n";
        List.sort updated_worry_list ~compare:(fun (a, _) (b, _) -> Int.compare a b)
        |> List.group ~break:(fun (a, _) (b, _) -> a <> b)
        |> List.iter ~f:(fun l ->
               let monkey_id = List.hd_exn l |> fst in
               let worries = List.map l ~f:(fun (_, worry) -> string_of_int worry) |> String.concat ~sep:", " in
               Printf.printf "Monkey %d: %s\n" monkey_id worries);*)
        (*Int.Map.to_alist additional_worry_counts |> List.iter ~f:(fun (monkey_id, worry_count) -> Printf.printf "Monkey %d: %d\n" monkey_id worry_count); *)

        (updated_worry_list, List.concat [additional_worry_counts; worry_counts])) in
  let [@warning "-8"] a::b::_ =
    final_worry_counts
    |> List.fold_left ~init:Int.Map.empty ~f:(fun worry_counts additional_worry_counts ->
           Int.Map.merge worry_counts additional_worry_counts ~f:(fun ~key:_ m ->
             let (a, b) = Map.Merge_element.values m ~left_default:0 ~right_default:0 in
             Some (a + b)))
    |> Int.Map.to_alist
    |> List.map ~f:snd
    |> List.sort ~compare:(fun a b -> Int.compare b a) in
  let res = a * b in
  Printf.printf "Res: %d\n" res

let get_ceiling monkeys = List.map monkeys ~f:(fun {divisibility; _} -> divisibility)
                        |> List.fold_left ~init:1 ~f:( * )

let part_2 s =
  let monkeys = parse s in
  let ceiling = get_ceiling monkeys in
  let worry_list = get_start_list monkeys in
  let worry_counts worry_list =
    List.map worry_list ~f:(fun (monkey_id, _) -> (monkey_id, ()) ) |> Int.Map.of_alist_fold ~init:0 ~f:(fun a _ -> a + 1 ) in
  let apply_operation operation worry =
    match operation with
    | Add n -> worry + n
    | Mul n -> worry * n
    | Square -> worry * worry in
  let monkey_moves { monkey_id; operation; divisibility; if_true; if_false; _ } worry_list =
    let (current_worries, rest) =  List.partition_tf worry_list  ~f:(fun (i, _) -> i = monkey_id) in
    
    let updated_worries =
      List.map current_worries ~f:(fun (_, worry) ->
          let inspected_worry = apply_operation operation worry in
          let shrunken_worry = inspected_worry mod ceiling in
          if shrunken_worry mod divisibility = 0 then
            (if_true, shrunken_worry)
          else
            (if_false, shrunken_worry)
        ) in
    (List.concat [rest; updated_worries],
     worry_counts current_worries) in
  let rounds = List.range 0 10000 |> List.map ~f:(fun _ -> monkeys) in
  let run_round monkeys worry_list =
    List.fold_left monkeys ~init:(worry_list, []) ~f:(fun (worry_list, worry_counts) monkey ->
      let (updated_worry_list, additional_worry_counts) = monkey_moves monkey worry_list in
      (updated_worry_list, additional_worry_counts::worry_counts)) in
  let (_, final_worry_counts) =
    List.fold_left rounds ~init:(worry_list, []) ~f:(fun (worry_list, worry_counts) monkeys ->
        let (updated_worry_list, additional_worry_counts) = run_round monkeys worry_list in
        (*Printf.printf "\nRound:\n";
        List.sort updated_worry_list ~compare:(fun (a, _) (b, _) -> Int.compare a b)
        |> List.group ~break:(fun (a, _) (b, _) -> a <> b)
        |> List.iter ~f:(fun l ->
               let monkey_id = List.hd_exn l |> fst in
               let worries = List.map l ~f:(fun (_, worry) -> string_of_int worry) |> String.concat ~sep:", " in
               Printf.printf "Monkey %d: %s\n" monkey_id worries);*)
        (*Int.Map.to_alist additional_worry_counts |> List.iter ~f:(fun (monkey_id, worry_count) -> Printf.printf "Monkey %d: %d\n" monkey_id worry_count); *)

        (updated_worry_list, List.concat [additional_worry_counts; worry_counts])) in
  let [@warning "-8"] a::b::_ =
    final_worry_counts
    |> List.fold_left ~init:Int.Map.empty ~f:(fun worry_counts additional_worry_counts ->
           Int.Map.merge worry_counts additional_worry_counts ~f:(fun ~key:_ m ->
             let (a, b) = Map.Merge_element.values m ~left_default:0 ~right_default:0 in
             Some (a + b)))
    |> Int.Map.to_alist
    |> List.map ~f:snd
    |> List.sort ~compare:(fun a b -> Int.compare b a) in
  let res = a * b in
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
