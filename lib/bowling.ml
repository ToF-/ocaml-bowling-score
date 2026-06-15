let score roll_list =
  let rolls = Array.of_list roll_list in
  let factors = Array.init (Array.length rolls) (fun _ -> 1) in
  let increment index = factors.(index) <- factors.(index) + 1 in
  let decrement index = factors.(index) <- factors.(index) - 1 in
  let rec collect_factors frame index rolls =
    match rolls with
    | [] -> ()
    | a :: rest when frame >= 10 ->
        decrement index;
        collect_factors frame (succ index) rest
    | a :: b :: c :: rest when a = 10 ->
        increment (succ index);
        increment (succ (succ index));
        collect_factors (succ frame) (succ index) (b :: c :: rest)
    | a :: b :: c :: rest when a + b = 10 ->
        increment (succ (succ index));
        collect_factors (succ frame) (succ (succ index)) (c :: rest)
    | a :: b :: rest -> collect_factors (succ frame) (succ (succ index)) rest
    | [ a ] -> ()
  in
  let result = ref 0 in
  collect_factors 0 0 roll_list;
  Array.iter2
    (fun roll factors -> result := !result + (roll * factors))
    rolls factors;
  !result
