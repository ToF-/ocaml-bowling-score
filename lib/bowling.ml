let score rolls =
    let roll = Array.of_list rolls in
    let factor = Array.init (List.length rolls) (fun _ -> 1) in
    let rec collect_factors f i rolls = match rolls with
    | (a::rest) when f >= 10 ->
            factor.(i) <- factor.(i) - 1;
            collect_factors f (i + 1) rest
    | [] -> ()
    | (a::b::c::rest) when a = 10 ->
            factor.(i+1) <- factor.(i+1) + 1;
            factor.(i+2) <- factor.(i+2) + 1;
            collect_factors (f + 1) (i + 1) (b::c::rest)
    | (a::b::c::rest) when a + b = 10 -> factor.(i+2) <- factor.(i+2) + 1 ; collect_factors (f + 1) (i + 2) (c::rest)
    | (a::b::rest) -> collect_factors (f + 1) (i + 2) rest
    | [a] -> ()
    in
    let result = ref 0 in
    collect_factors 0 0 rolls ;
    Array.iter2 (fun r f -> result := !result + r * f) roll factor ;
    !result
