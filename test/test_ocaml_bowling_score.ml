open OUnit2
open Printf
open Ocaml_bowling_score.Bowling

let tests = "bowling score" >:::
    ["given no rolls then score is zero" >:: fun _ ->
        assert_equal ~printer: string_of_int 
        0 (score [])
    ]

let _ = run_test_tt_main tests

