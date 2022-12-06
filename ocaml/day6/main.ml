open Core

let small_input =  "mjqjpqmgbljsphdztnvjfqwrcgsmlb"

let big_input = In_channel.read_all "day6/input.txt"

         
let test s l =
  let m = Int.Map.empty in
  let g i  = (String.get s i |> int_of_char) - 97 in
  let incr a m=
    let n = Int.Map.find m a in
    match n with
    | None -> Int.Map.set m ~key:a ~data:1
    | Some n -> Int.Map.set m ~key:a ~data:(n+1) in
  let decr a m =
    let n = Int.Map.find_exn m a in
    if n = 1 then
      Int.Map.remove m a
    else begin
        assert (n > 1);
        Int.Map.set m ~key:a ~data:(n-1)
      end in
  let rec fill_init m i =
    if i < (l - 1) then
      fill_init (incr (g i) m) (i + 1)
    else m in
  let m = fill_init m 0 in
  let rec loop m a =
    let m = incr (g a) m in
    assert (Int.Map.length m <= l);
    assert (Int.Map.length m >= 1);
    assert ((Int.Map.to_alist m |> List.map ~f:snd |> List.sum (module Int) ~f:Fn.id) = l);
    if Int.Map.length m = l then
      a
    else
      let m = decr (g (a - (l-1))) m in
      loop m (a + 1) in
  loop m (l - 1) + 1


let part_1 s =
  let res = test s 4 in
  Printf.printf "Res: %d\n" res
  
let part_2 s =
  let res = test s 14 in
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
 
   
