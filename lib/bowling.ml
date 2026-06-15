let score rolls =
    let roll = Array.of_list rolls in
    let factor = Array.init (List.length rolls) (fun _ -> 1) in
    let rec collect_factors f i rolls = match rolls with
    | [] -> ()
    | (a::b::c::rest) when a = 10 -> factor.(i+1) <- factor.(i+1) + 1 ; factor.(i+2) <- factor.(i+2) + 1; collect_factors (f + 1) (i + 1) (b::c::rest)
    | (a::b::c::rest) when a + b = 10 -> factor.(i+2) <- factor.(i+2) + 1 ; collect_factors (f + 1) (i + 2) (c::rest)
    | (a::b::rest) -> if f >= 10 then factor.(i) <- factor.(i) - 1 ; collect_factors (f + 1) (i + 2) rest
    | (a::rest) -> if f >= 10 then factor.(i) <- factor.(i) - 1; collect_factors (f + 1) (i + 1) rest
    in
    let result = ref 0 in
    collect_factors 0 0 rolls ;
    Array.iter2 (fun r f -> result := !result + r * f) roll factor ;
    !result
