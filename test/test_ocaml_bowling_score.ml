open OUnit2
open Printf
open Ocaml_bowling_score.Bowling

let tests = "bowling score" >:::
    [("given no rolls then score is zero" >:: fun _ ->
        assert_equal ~printer: string_of_int 0 (score []));
    ("given one roll then score is that roll" >:: fun _ ->
        assert_equal ~printer: string_of_int 7 (score [7]));
    ("given several average rolls then score is the sum of these rolls" >:: fun _ ->
        assert_equal ~printer: string_of_int 10 (score [3;2;4;1]));
    ("given a spare then the next roll is added as bonus" >:: fun _ ->
        assert_equal ~printer: string_of_int 14 (score [4;6;2]));
    ("given a strike then the two next rolls are added as bonus" >:: fun _ ->
        assert_equal ~printer: string_of_int 26 (score [10;4;3;2]));
    ("a strike is valid on a new frame" >:: fun _ ->
        assert_equal ~printer: string_of_int 21 (score [0;10;4;2;1]));
    ("after 10 frames rolls are not added, only bonus" >:: fun _ ->
        assert_equal ~printer: string_of_int 300 (score [10;10;10;10;10;10;10;10;10;10;10;10]));
    ]

let _ = run_test_tt_main tests

