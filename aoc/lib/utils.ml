(* Read entire file into a single string *)
let read_all filename = 
    In_channel.with_open_text filename In_channel.input_all

(* Absolute remainder (similiar to % in python)*)
let ( %%% ) a b =
  ((a mod b) + b) mod b



let f () = let y = 0 in y




