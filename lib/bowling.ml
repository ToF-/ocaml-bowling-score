let score rolls =
    let roll = Array.of_list rolls in
    let factor = Array.init (List.length rolls) (fun _ -> 1) in
    let rec collect_factors i rolls = match rolls with
    | [] -> ()
    | (a::b::c::rest) when a + b = 10 -> factor.(i+2) <- factor.(i+2) + 1 ; collect_factors (i + 2) (c::rest)
    | (a::rest) -> collect_factors (i + 1) rest
    in
    let result = ref 0 in
    collect_factors 0 rolls ;
    Array.iter2 (fun r f -> result := !result + r * f) roll factor ;
    !result
