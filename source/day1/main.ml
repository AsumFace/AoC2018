open Core

let lines = In_channel.read_lines "../../views/day1_input"

(* solution 1 *)
let nums = List.map ~f:(int_of_string) lines
let result = List.fold ~f:(+) nums ~init:0
let () = printf "%d\n" result

(* solution 2 *)
let () = List.map ~f:(int_of_string) lines
    |> List.fold ~f:(+) ~init:0
    |> printf "%d\n"