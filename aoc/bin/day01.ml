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


let turn (position, count) instruction = 
    let new_pos = match instruction.dir with
        | 'L' -> (position - instruction.value) %%% 100
        | 'R' -> (position + instruction.value) %%% 100
        | _ -> failwith "Unknown instruction" in
    let new_count = 
        if new_pos = 0 then count + 1 else count in
    (new_pos, new_count)



let () = 
    let instructions = read_file "inputs/day01.txt" in
    let final_position, count = List.fold_left turn (50, 0) instructions in
    Printf.printf "Final position: %d and count: %d\n" final_position count



