open Core

let small_input = {|    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2|}


  
let big_input = In_channel.read_all "day5/input.txt" |> (String.strip ~drop:Char.(fun c -> c = '\n'))

let is_digit d =
  Char.('0' <= d && d <= '9')

let rec read_stack lines index =
  match lines with
  | line::rest ->
     let open Char in
     let ch = String.get line (4 * index + 1) in
     if ch = ' ' then
       read_stack rest index
     else
       ch :: read_stack rest index
  | [] -> []
    
let read_stacks lines =
  let stack_count = ((List.hd_exn lines |> String.length) + 1) / 4 in  
  let rec loop i =
    if i = stack_count then
      []
    else
      read_stack lines i :: loop (i + 1) in
  loop 0


let re = Re.Perl.compile_pat "move (\\d+) from (\\d+) to (\\d+)"  
let parse s =
  let lines = String.split ~on:'\n' s in
  let (box_lines, rest)  = List.split_while lines ~f:(fun line -> not (is_digit (String.get line 1))) in
  
  let [@warning "-8"] (_::_::step_lines) = rest in
  let stacks = read_stacks box_lines in
  let steps = List.map step_lines ~f:(fun line ->
                  let g = Re.exec re line in
                  let rg i = (Re.Group.get g i |> int_of_string) in
                  (rg 1, rg 2 - 1, rg 3 - 1)) in
  (stacks, steps)


let rec move src dest c =
  if c = 0 then
    (src, dest)
  else
    let [@warning "-8"] x::xs = src in
    move xs (x::dest) (c - 1)
    


let part_1 s =
  let (stacks, steps) = parse s in
  let arr = List.to_array stacks in
  List.iter steps ~f:(fun (count, src_i, dest_i) ->
      let src = Array.get arr src_i in
      let dest = Array.get arr dest_i in
      let (src, dest) = move src dest count in
      Array.set arr src_i src;
      Array.set arr dest_i dest);
  let res = Array.map arr ~f:List.hd_exn |> Array.to_list |> String.of_char_list in
  Printf.printf "Res: %s\n" res 
          

let move2 (count, dest, src) (stack, depth) =
  if src = stack && count <= depth then
    (src, depth - count)
  else if src = stack then
    (dest, depth)
  else if dest = stack then
    (stack, depth+count)
  else
    (stack, depth)
    

let part_2 s =
  let (stacks, steps) = parse s in
  let coords = List.range 0 (List.length stacks) |>
    List.map ~f:(fun idx ->
        List.fold_right steps ~f:move2 ~init:(idx, 0)) in
  let res = List.map coords ~f:(fun (stack, depth) -> List.nth_exn (List.nth_exn stacks stack) depth)
            |> String.of_char_list in
  Printf.printf "Res: %s\n" res
  
  

  
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
     
   
