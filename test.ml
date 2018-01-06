(* >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
 *
 *              This is the test suite for model
 *
 * <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< *)
open OUnit2
open Board



let king (x:piece_color) =
    {
      color = x;
      piece_type = King;
      piece_value = 10000;
      piece_direction = get_dir King x;
      last_move = None;
    }


let queen (x:piece_color) =
  {
    color = x;
    piece_type = Queen;
    piece_value = 900;
    piece_direction = get_dir Queen x;
    last_move = None;
  }

let rook (x:piece_color) =
  {
    color = x;
    piece_type = Rook;
    piece_value = 500;
    piece_direction = get_dir Rook x;
    last_move = None;
  }

let bishop (x:piece_color) =
  {
    color = x;
    piece_type = Bishop;
    piece_value = 300;
    piece_direction = get_dir Bishop x;
    last_move = None;
  }

let knight (x:piece_color) =
  {
    color = x;
    piece_type = Knight;
    piece_value = 300;
    piece_direction = get_dir Knight x;
    last_move = None;
  }

let pawn_w =
  {
    color = White;
    piece_type = Pawn;
    piece_value = 100;
    piece_direction = get_dir Pawn White;
    last_move = None;
  }

let pawn_b =
  {
    color = Black;
    piece_type = Pawn;
    piece_value = 100;
    piece_direction = get_dir Pawn Black;
    last_move = None;
  }

let x = ref 3
let y = ref 3
let init_boards = init_board ()

let init_board =  [
  "none33" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (3,3)));
  "none34" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (3,4)));
  "none35" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (3,5)));
  "none36" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (3,6)));
  "none43" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (4,3)));
  "none44" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (4,4)));
  "none45" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (4,5)));
  "none46" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (4,6)));
  "none53" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (5,3)));
  "none54" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (5,4)));
  "none55" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (5,5)));
  "none56" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (5,6)));
  "none63" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (6,3)));
  "none64" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (6,4)));
  "none65" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (6,5)));
  "none66" >:: (fun _ -> assert_equal None (Hashtbl.find init_boards (6,6)));
  "BKing" >:: (fun _ -> assert_equal (Some (king Black)) (Hashtbl.find init_boards (5,8)));
  "WKing" >:: (fun _ -> assert_equal (Some (king White)) (Hashtbl.find init_boards (5,1)));
  "BQueen" >:: (fun _ -> assert_equal (Some (queen Black)) (Hashtbl.find init_boards (4,8)));
  "WQueen" >:: (fun _ -> assert_equal (Some (queen White)) (Hashtbl.find init_boards (4,1)));
  "BRook1" >:: (fun _ -> assert_equal (Some (rook Black)) (Hashtbl.find init_boards (1,8)));
  "BRook2" >:: (fun _ -> assert_equal (Some (rook Black)) (Hashtbl.find init_boards (8,8)));
  "WRook1" >:: (fun _ -> assert_equal (Some (rook White)) (Hashtbl.find init_boards (1,1)));
  "WRook2" >:: (fun _ -> assert_equal (Some (rook White)) (Hashtbl.find init_boards (8,1)));
  "BKnight1" >:: (fun _ -> assert_equal (Some (knight Black)) (Hashtbl.find init_boards (2,8)));
  "BKnight2" >:: (fun _ -> assert_equal (Some (knight Black)) (Hashtbl.find init_boards (7,8)));
  "WKnight1" >:: (fun _ -> assert_equal (Some (knight White)) (Hashtbl.find init_boards (2,1)));
  "WKinght2" >:: (fun _ -> assert_equal (Some (knight White)) (Hashtbl.find init_boards (7,1)));
  "WBishop1" >:: (fun _ -> assert_equal (Some (bishop Black)) (Hashtbl.find init_boards (3,8)));
  "BBishop2" >:: (fun _ -> assert_equal (Some (bishop Black)) (Hashtbl.find init_boards (6,8)));
  "WBishop1" >:: (fun _ -> assert_equal (Some (bishop White)) (Hashtbl.find init_boards (3,1)));
  "WBishop2" >:: (fun _ -> assert_equal (Some (bishop White)) (Hashtbl.find init_boards (6,1)));
  ]

let init_pawns x = [
    "WPawn"^string_of_int x >:: (fun _ -> assert_equal (Some (pawn_w)) (Hashtbl.find init_boards (x,2)));
    "BPawn"^string_of_int x >:: (fun _ -> assert_equal (Some (pawn_b)) (Hashtbl.find init_boards (x,7)));
  ]

let st = init_state ()
let get_piece_t pos st = get_piece st pos
let wPn1 = get_piece_t (4,2) st |> piece_from_op
let bPn1 = get_piece_t (4,7) st |> piece_from_op
let mv1  = Normal ((4,2),(4,3))
let mv2 = Normal ((4,2), (4,4))
let mv3 = Normal ((4,7),(4,6))
let mv4 = Normal ((4,7),(4,5))

let wN = get_piece_t (2,1) st |> piece_from_op
let mv5 = Normal((2,1),(3,3))
let mv6 = Normal((2,1),(1,3))

let move_t mv p o st = move st mv p o
let wK = get_piece_t (5,1) st |> piece_from_op
let mv7 = Castle_Left
let mv8 = Castle_Right

let bK = get_piece_t (5,8) st |> piece_from_op
let mv9 = Castle_Left
let mv10 = Castle_Right

let wR1 = get_piece_t (1,1) st |> piece_from_op
let wR2 = get_piece_t (8,1) st |> piece_from_op
let bR1 = get_piece_t (1,8) st |> piece_from_op
let bR2 = get_piece_t (8,8) st |> piece_from_op
let mv11 = Normal ((1,1), (4,1))
let mv12 = Normal ((1,8), (4,8))

let wB = get_piece_t (3,1) st |> piece_from_op
let mv13 = Normal ((3,1),(8,6)) (* Bishop move *)

let wQ = get_piece_t (4,1) st |> piece_from_op
let mv14 = Normal ((4,1), (4,6))
let mv15 = Normal ((4,6), (7,3))

let wP2 = get_piece_t (3,2) st |> piece_from_op
let mp1 = Normal ((3,7),(3,8))


let test_moves x =
  [
    "Pawn move" >:: (fun _ -> assert_equal wPn1 (move st mv1 wPn1 None
                                                  |> fst |> get_piece_t (4,3) |> piece_from_op));
    "Pawn jump" >:: (fun _ -> assert_equal wPn1 (move st mv2 wPn1 None
                                                  |> fst |> get_piece_t (4,4) |> piece_from_op));
    "Pawn move2" >:: (fun _ -> assert_equal bPn1 (move st mv3 bPn1 None
                                                  |> fst |> get_piece_t (4,6) |> piece_from_op));
    "Pawn jump2" >:: (fun _ -> assert_equal bPn1 (move st mv4 bPn1 None
                                                  |> fst |> get_piece_t (4,5) |> piece_from_op));
    "Knight move1" >:: (fun _ -> assert_equal wN (move st mv5 wN None
                                                  |> fst |> get_piece_t (3,3) |> piece_from_op));
    "Knight move2" >:: (fun _ -> assert_equal wN (move st mv6 wN None
                                                  |> fst |> get_piece_t (1,3) |> piece_from_op));

    "wCastle left" >:: (fun _ -> assert_equal wK (st |> remove (2,1) |> remove (3,1) |> remove (4,1) |> move_t mv7 wK None
                                                  |> fst |> get_piece_t (3,1) |> piece_from_op));
    "wCastle right" >:: (fun _ -> assert_equal wK (st |> remove (7,1) |> remove (6,1) |> move_t mv8 wK None
                                                  |> fst |> get_piece_t (7,1) |> piece_from_op));
    "wCastle left_r" >:: (fun _ -> assert_equal wR1 (st |> remove (2,1) |> remove (3,1) |> remove (4,1) |> move_t mv7 wK None
                                                  |> fst |> get_piece_t (4,1) |> piece_from_op));
    "wCastle right_r" >:: (fun _ -> assert_equal wR2 (st |> remove (7,1) |> remove (6,1) |> move_t mv8 wK None
                                                  |> fst |> get_piece_t (6,1) |> piece_from_op));


    "bCastle left2" >:: (fun _ -> assert_equal wK (st |> remove (2,8) |> remove (3,8) |> remove (4,8) |> move_t mv9 wK None
                                                  |> fst |> get_piece_t (3,8) |> piece_from_op));
    "bCastle right2" >:: (fun _ -> assert_equal wK (st |> remove (7,8) |> remove (6,8) |> move_t mv10 wK None
                                                  |> fst |> get_piece_t (7,8) |> piece_from_op));
    "bCastle left2_r" >:: (fun _ -> assert_equal bR1 (st |> remove (2,8) |> remove (3,8) |> remove (4,8) |> move_t mv9 wK None
                                                  |> fst |> get_piece_t (4,8) |> piece_from_op));
    "bCastle right2_r" >:: (fun _ -> assert_equal bR2 (st |> remove (7,8) |> remove (6,8) |> move_t mv10 wK None
                                                  |> fst |> get_piece_t (6,8) |> piece_from_op));

    "white Rook Move" >:: (fun _ -> assert_equal wR1 (st |> remove (2,1) |> remove (3,1) |> remove (4,1) |> move_t mv11 wR1 None
                                                  |> fst |> get_piece_t (4,1) |> piece_from_op));
    "black Rook Move" >:: (fun _ -> assert_equal bR1 (st |> remove (2,8) |> remove (3,8) |> remove (4,8) |> move_t mv12 bR1 None
                                                  |> fst |> get_piece_t (4,8) |> piece_from_op));

    "Bishop move1" >:: (fun _ -> assert_equal wB (st |> remove (4,2) |> move_t mv13 wB None
                                                    |> fst |> get_piece_t (8,6) |> piece_from_op));

    "Queen move straight" >:: (fun _ -> assert_equal wQ (st |> remove (4,2) |> move_t mv14 wQ None
                                                    |> fst |> get_piece_t (4,6) |> piece_from_op));
    "Queen move diagonal" >:: (fun _ -> assert_equal wQ (st |> remove (4,2) |> move_t mv14 wQ None
                                                    |> fst |> move_t mv4 bPn1 None
                                                    |> fst |> move_t mv15 wQ None
                                                    |> fst |> get_piece_t (7,3) |> piece_from_op));

    "Promotion" >:: (fun _ -> assert_equal (Queen) (st |> remove (3,7) |> remove (3,8)
                                                  |> addP (3,7) (Some wP2) |> move_t mp1 wP2 (Some "Queen")
                                                  |> fst |> get_piece_t (3,8)
                                                  |> piece_from_op |> get_piece_type))

  ]

let rec check_pawns accu = function
  | 9 -> accu
  | x -> check_pawns ((x|>init_pawns)@accu) (x+1)

let pawns = check_pawns [] 1

let suite = "A3 test suite" >:::
  init_board @
  pawns (*)@
          test_moves 1*)
(*   test_moves *)



let _ = run_test_tt_main suite
