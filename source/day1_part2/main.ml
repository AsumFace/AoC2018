open Core

(* solution 1 *)
let stdin = In_channel.create "../../views/day1_input"

let fchanges =
    (Sequence.unfold ~init:0
        ~f:(fun _ -> match (In_channel.input_line stdin) with
            | Some line -> let f = int_of_string line in Some (f, f)
            | None -> In_channel.seek stdin 0L;
                let f = Option.value_exn (In_channel.input_line stdin) |> int_of_string in Some(f, f)))

let fnums = Sequence.unfold_with fchanges ~init:0
    ~f:(fun a deltaf -> Yield (a, a + deltaf))

let rec find_dup ?set seq =
    let set = match set with Some s -> s | None -> Set.empty (module Int) in
    let head, seq = (match Sequence.next seq with Some e -> e | None -> failwith "end of sequence reached") in
    if Set.mem set head then
        head
    else
        let set = Set.add set head in
        find_dup ~set:set seq

let () = find_dup fnums |> printf "%d\n"