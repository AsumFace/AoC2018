open Core

(* solution 1 *)
(* looks overcomplicated AF *)
let input_file =
    let mapping = Unix.map_file
        (Unix.openfile ~mode:[Unix.O_RDONLY]  "../../views/day2_input")
        Char
        Bigarray.c_layout
        ~shared:false
        [|-1|] in
    Bigarray.reshape_1 mapping (Bigarray.Genarray.size_in_bytes mapping)

let sequence_of_bigarray barr =
    Sequence.unfold ~init:barr
        ~f:(fun state ->
            if Bigarray.Array1.size_in_bytes state > 0 then
                Some (state.{0}, Bigarray.Array1.sub state 1 (Bigarray.Array1.size_in_bytes state - 1))
            else
                None)

let sequence_to_eol seq =
    Sequence.take_while ~f:(fun c -> match c with '\n' -> false | _ -> true) seq

let sequence_lines barr =
    let sob = sequence_of_bigarray barr in
    let empty = sequence_to_eol (Sequence.of_list ['\n']) in
    Sequence.unfold ~init:(empty, sob)
        ~f:(fun (prev, rem) ->
            match Sequence.next rem with
            | Some _ ->
                let rem = Sequence.drop_eagerly rem (Sequence.count prev ~f:(fun _ -> true)) in
                Some (sequence_to_eol rem, (sequence_to_eol rem, rem))
            | None -> None)

let get_match str1 str2 =
    let () = assert ((Sequence.count ~f:(fun _ -> true) str1) = (Sequence.count ~f:(fun _ -> true) str2)) in
    let overlap = Sequence.zip str1 str2 |> Sequence.filter_map ~f:(fun (a, b) -> if a = b then Some a else None) in
    ((Sequence.count ~f:(fun _ -> true) str1) - (Sequence.count ~f:(fun _ -> true) overlap), overlap)

let comp_thereafter ~comp_fun seq =
    printf "%d " (Sequence.count ~f:(fun _ -> true) seq);
    let first, seq = Option.value_exn (Sequence.next seq) in
    Sequence.unfold ~init:(first, seq)
        ~f:(fun (lhs, remainder) ->
            if (Sequence.count ~f:(fun _ -> true) seq) > 1 then
                let rhs, remainder = Option.value_exn (Sequence.next remainder) in
                Some (comp_fun lhs rhs, (first, remainder))
            else
                None)

let drop_sequence ~min seq =
    Sequence.unfold ~init:seq
        ~f:(fun seq ->
            printf "%d " (Sequence.count ~f:(fun _ -> true) seq);
            printf "%d " (Sequence.count ~f:(fun _ -> true) seq);
            let _, remainder = Option.value_exn (Sequence.next seq) in
            printf "%d " (Sequence.count ~f:(fun _ -> true) remainder);
            if Sequence.count ~f:(fun _ -> true) seq >= min then
                Some (seq, remainder)
            else
                None)

let print_charseq seq =
    Sequence.iter seq ~f:(printf "%c")

let box_ids = sequence_lines input_file

let find_solution seq =
    Sequence.iter
        (drop_sequence ~min:2 seq
            |> Sequence.map ~f:(comp_thereafter ~comp_fun:get_match)
            |> Sequence.concat)
        ~f:(fun m -> match m with
            | (2, r) -> print_charseq r
            | _ -> ())

let () = find_solution box_ids
