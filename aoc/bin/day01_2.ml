open Aoc.Utils

type instr = {
    dir: char;
    value: int;
}
let parse line =
        let dir = line.[0] in 
        let value = String.sub line 1 (String.length line - 1) |> int_of_string in
        {dir; value}


let read_file filename =
        In_channel.with_open_text filename @@ fun ic ->
        In_channel.input_lines ic |> List.map parse

let n = 100

let hits_zero pos dir v =
(* we want to see how much we need to hit the first zero first*)
(* this depends on where we start, e.g. if we start from 0 we need n ticks to hit the 
   first zero ... we then check if we have enough ticks (in v) to hit the first 0 
   and if we do we can start from 0  and count how many wraps we can achieve with the 
   rests (i.e. v - first) / n*)
  let first =
    match dir with 
    | 'L' -> if pos = 0 then n else pos
    | 'R' -> if pos = 0 then n else n - pos
    | _ -> failwith "Unknown instruction" in 
  if v < first then 0 else 1 + (v - first) / n 


let turn (position, count) instruction =
  let v = instruction.value in
  let hits = hits_zero position instruction.dir v in
  let new_pos =
    match instruction.dir with
    | 'R' -> (position + v) %%% n
    | 'L' -> (position - v) %%% n
    | _ -> failwith "Unknown instruction"
  in
  (new_pos, count + hits)

let () = 
    let instructions = read_file "inputs/day01_2.txt" in
    let final_position, count = List.fold_left turn (50, 0) instructions in
    Printf.printf "Final position: %d and count: %d\n" final_position count


