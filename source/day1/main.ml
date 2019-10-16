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

(* solution 3 *)
let () = lines
    |> List.fold ~f:(fun a b -> a + (int_of_string b)) ~init:0
    |> printf "%d\n"

(* solution 4 *)
let () = In_channel.fold_lines (In_channel.create "../../views/day1_input")
    ~init:0
    ~f:(fun a b -> a + (int_of_string b))
    |> printf "%d\n"

(* solution 5 *)
let stdin = In_channel.create "../../views/day1_input"

let fchanges = Sequence.append
    (Sequence.unfold ~init:0
        ~f:(fun _ -> match (In_channel.input_line stdin) with
            | Some line -> let f = int_of_string line in Some (f, f)
            | None -> None))
    (Sequence.singleton 0)
let fnums = Sequence.unfold_with fchanges ~init:0
    ~f:(fun a deltaf -> Yield (a, a + deltaf))
let () = Sequence.fold ~init:0 ~f:(fun _ b -> b) fnums |> printf "%d\n"