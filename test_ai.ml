open OUnit2
open AI
open Board

let init_state= init_state ()
let init_color = init_state|>get_turn_color

let init = [
  "number" >:: (fun _ -> assert_equal 20 (List.length (get_frontier init_state init_color)));
  "piecesW" >:: (fun _ -> assert_equal 16 (List.length (get_pieces init_state init_color)));
  "piecesB" >:: (fun _ -> assert_equal 16 (List.length (get_pieces init_state (init_color|>get_next))));
  "WKightScore" >:: (fun _ -> assert_equal 0 (get_pscore (5,1) Early White King));
  "BKightScore" >:: (fun _ -> assert_equal 0 (get_pscore (5,8) Early Black King));

  "utility_W" >:: (fun _ -> assert_equal (13820) (utility init_state init_color));
  "utility_B" >:: (fun _ -> assert_equal (13820) (utility init_state (get_next init_color)));


  ]

let suite = "AI test suite" >::: init

let _ = run_test_tt_main suite
