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

let print_charseq seq =
    Sequence.iter seq ~f:(printf "%c")

let sequence_to_eol seq =
    Sequence.take_while ~f:(fun c -> match c with '\n' -> false | _ -> true) seq

let yes _ = true

let cnt = ref 0

let rec drop_to_nextline seq =
    let rec drop_newlines seq =
        let next = Sequence.next seq in
        match next with
        | None -> Sequence.drop_eagerly seq 1
        | Some (head, tail) -> match head with
            | '\n' -> drop_newlines tail
            | _ -> seq
            in
    let next = Sequence.next seq in
    match next with
    | None -> Sequence.drop_eagerly seq 1
    | Some (head, tail) -> match head with
        | '\n' -> drop_newlines seq
        | _ -> drop_to_nextline tail

let sequence_lines barr =
    let sob = sequence_of_bigarray barr in
    Sequence.unfold ~init:sob
        ~f:(fun (rem) ->
            match Sequence.next rem with
            | Some _ ->
                let rem = drop_to_nextline rem in
                Some (sequence_to_eol rem, rem)
            | None -> None)

let get_match str1 str2 =
    let () = assert ((Sequence.count ~f:yes str1) = (Sequence.count ~f:yes str2)) in
    let overlap = Sequence.zip str1 str2 |> Sequence.filter_map ~f:(fun (a, b) -> if a = b then Some a else None) in
    ((Sequence.count ~f:yes str1) - (Sequence.count ~f:yes overlap), overlap)

let comp_thereafter ~comp_fun ~filter seq =
    let first, seq = Option.value_exn (Sequence.next seq) in
    let () = assert (Sequence.count ~f:yes first |> (=) 26) in
    Sequence.unfold ~init:(first, seq)
        ~f:(let rec rg (lhs, remainder) =
            if (Sequence.count ~f:yes remainder) > 1 then
                let rhs, remainder = Option.value_exn (Sequence.next remainder) in
                if filter rhs then
                    Some (comp_fun lhs rhs, (first, remainder))
                else
                    rg (first, remainder)
            else
                None
            in rg)

let drop_sequence ~min seq =
    Sequence.unfold ~init:seq
        ~f:(fun seq ->
            let _, remainder = Option.value_exn (Sequence.next seq) in
            if Sequence.count ~f:yes seq >= min then
                Some (seq, remainder)
            else
                None)

let box_ids = sequence_lines input_file
let find_solution seq =
    Sequence.iter
        (drop_sequence ~min:2 seq
            |> Sequence.map
                ~f:(comp_thereafter
                    ~comp_fun:get_match
                    ~filter:(fun rhs -> Sequence.bounded_length rhs ~at_most:0 = `Greater))
            |> Sequence.concat)
        ~f:(fun m -> match m with
            | (1, r) -> print_charseq r; printf "\n"
            | _ -> ())

let () = find_solution box_ids
