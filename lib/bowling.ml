let rec score = function
| [] -> 0
| l ->  List.hd l + (score (List.tl l))
