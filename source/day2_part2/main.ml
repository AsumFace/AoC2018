open Core

(* solution 1 *)
(* I do not like this algorithm *)
let stdin = In_channel.create "../../views/day2_input"

(* WTF, why does Core not provide stuff like this? *)
let sequence_of_string str =
    Sequence.unfold ~init:str ~f:(fun state ->
        match String.length state with
            | 0 -> None
            | _ -> Some (String.get state 0, String.sub ~pos:1 ~len:((String.length state) - 1) state))

let box_ids =
    (Sequence.unfold ~init:()
        ~f:(fun _ -> match (In_channel.input_line stdin) with
            | Some line -> Some (sequence_of_string line, ())
            | None -> None))

let get_len seq = Sequence.fold ~init:0 ~f:(fun acc  _ -> acc + 1) seq

let get_match str1 str2 =
    let () = assert ((get_len str1) = (get_len str2)) in
    let overlap = Sequence.zip str1 str2 |> Sequence.filter_map ~f:(fun (a, b) -> if a = b then Some a else None) in
    ((get_len str1) - (get_len overlap), overlap)

let comp_thereafter ~comp_fun seq =
    printf "%d " (get_len seq);
    let first, seq = Option.value_exn (Sequence.next seq) in
    Sequence.unfold ~init:(first, seq)
        ~f:(fun (lhs, remainder) ->
            if (get_len seq) > 1 then
                let rhs, remainder = Option.value_exn (Sequence.next remainder) in
                Some (comp_fun lhs rhs, (first, remainder))
            else
                None)

let drop_sequence ~min seq =
    Sequence.unfold ~init:seq
        ~f:(fun seq ->
            printf "%d " (get_len seq);
            printf "%d " (get_len seq);
            let _, remainder = Option.value_exn (Sequence.next seq) in
            printf "%d " (get_len remainder);
            if get_len seq >= min then
                Some (seq, remainder)
            else
                None)

let print_charseq seq =
    Sequence.iter seq ~f:(printf "%c")

let find_solution seq =
    Sequence.iter
        (drop_sequence ~min:2 seq
            |> Sequence.map ~f:(comp_thereafter ~comp_fun:get_match)
            |> Sequence.concat)
        ~f:(fun m -> match m with
            | (2, r) -> print_charseq r
            | _ -> ())

let () = find_solution box_ids
