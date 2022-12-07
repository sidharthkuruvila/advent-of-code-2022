open Core

let small_input = {|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k|}


  
let big_input = In_channel.read_all "day7/input.txt" |> (String.strip ~drop:Char.(fun c -> c = '\n'))


(* The horror! *)              
              
let find_sub_1ks lines =
  List.fold_right lines ~f:(fun line (sum_stack, candidates) ->
      let ch1 = String.get line 0 in
      if Char.is_digit ch1 then
        let [@warning "-8"] current_sum::rest = sum_stack in
        let [@warning "-8"] [ds; _] = String.split ~on:' ' line in
        let d = int_of_string ds in
        let current_sum = d + current_sum in
        (current_sum::rest, candidates)
      else if String.(line = "$ cd ..") then
        (0::sum_stack, candidates)
      else if String.(prefix line 5 = "$ cd ") then
        let [@warning "-8"] a::b::rest = sum_stack in
        (a+b::rest, if a <= 100000 then a + candidates else candidates)
      else
        (sum_stack, candidates)
    ) ~init:([0;0;0;0;0;0;0;0;0;0;0;0;0;0], 0)


let find_best min_size lines =
  List.fold_right lines ~f:(fun line (sum_stack, candidate) ->
      let ch1 = String.get line 0 in
      if Char.is_digit ch1 then
        let [@warning "-8"] current_sum::rest = sum_stack in
        let [@warning "-8"] [ds; _] = String.split ~on:' ' line in
        let d = int_of_string ds in
        let current_sum = d + current_sum in
        (current_sum::rest, candidate)
      else if String.(line = "$ cd ..") then
        (0::sum_stack, candidate)
      else if String.(prefix line 5 = "$ cd ") then
        let [@warning "-8"] a::b::rest = sum_stack in
        (a+b::rest, if a > min_size && a < candidate then  a else candidate)
      else
        (sum_stack, candidate)
    ) ~init:([0;0;0;0;0;0;0;0;0;0;0;0;0;0], Int.max_value)

let part_1 s =
  let res = String.strip s
            |> String.split ~on:'\n'
            |> find_sub_1ks
            |> snd in
  Printf.printf "Res: %d\n" res
        
  
let part_2 s =
  let lines = String.strip s
              |> String.split ~on:'\n' in
  let occupied_size = find_sub_1ks lines |> fst |> List.hd_exn in
  let required_size = occupied_size - 40000000 in
  let res = find_best required_size lines |> snd in
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
     
   
