let score rolls =
    let roll = Array.of_list rolls in
    let factor = Array.init (List.length rolls) (fun _ -> 1) in
    let rec collect_factors frame index rolls = match rolls with
    | [] ->
            ()
    | (a::rest) when frame >= 10 ->
            factor.(index) <- factor.(index) - 1 ;
            collect_factors frame (index + 1) rest
    | (a::b::c::rest) when a = 10 ->
            factor.(index+1) <- factor.(index+1) + 1 ;
            factor.(index+2) <- factor.(index+2) + 1 ;
            collect_factors (frame + 1) (index + 1) (b::c::rest)
    | (a::b::c::rest) when a + b = 10 ->
            factor.(index+2) <- factor.(index+2) + 1 ;
            collect_factors (frame + 1) (index + 2) (c::rest)
    | (a::b::rest) ->
            collect_factors (frame + 1) (index + 2) rest
    | [a] -> ()
    in
    let result = ref 0 in
    collect_factors 0 0 rolls ;
    Array.iter2 (fun r f -> result := !result + r * f) roll factor ;
    !result
