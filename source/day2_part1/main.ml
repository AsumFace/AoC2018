open Core

(* solution 1 *)
let stdin = In_channel.create "../../views/day2_input"
let box_ids =
    (Sequence.unfold ~init:()
        ~f:(fun _ -> match (In_channel.input_line stdin) with
            | Some line -> Some (line, ())
            | None -> None))

let char_histogram str =
    let counts = Array.init ~f:(fun _ -> 0) 256 in
    String.iter ~f:(fun ch -> counts.(Char.to_int ch) <- (counts.(Char.to_int ch) + 1)) str;
    counts

let () = box_ids
    |> Sequence.map
        ~f:(fun id -> char_histogram id
            |> Array.fold
                ~init:(Array.create ~len:2 0)
                ~f:(fun result num -> match num with
                    | 2 -> (result.(0) <- 1; result)
                    | 3 -> (result.(1) <- 1; result)
                    | _ -> result))
    |> Sequence.fold ~init:(Array.create ~len:2 0)
        ~f:(fun acc arr -> Array.iteri arr ~f:(fun i e -> acc.(i) <- acc.(i) + e); acc)
    |> fun a -> Array.fold ~init:1 ~f:( * ) a
    |> printf "%d\n"

