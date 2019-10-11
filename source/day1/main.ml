open Core

let lines = In_channel.read_lines "../../views/day1_input"
let nums = List.map ~f:(int_of_string) lines
let result = List.fold ~f:(+) nums ~init:0
let () = printf "%d" result