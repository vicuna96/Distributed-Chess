open Graphics


(* Define variables whose values are arrays representing the shape of
   all the chess pieces. *)

let king = [|(10,0);(10,25);(20,25);(20,30);(5,30);(5,35);(20,35);(20,45);
             (30,45);(30,35);(45,35);(45,30);(30,30);(30,25);(40,25);(40,0)|]

let pawn = [|(10,2);(10,24);(15,24);(15,47);(35,47);(35,24);(40,24);(40,2)|]

let queen = [|(15,0);(5,20);(10,20);(15,10);(10,45);(15,45);(20,25);(25,45);
              (30,45);(35,25);(40,45);(45,45);(40,10);(45,20);(50,20);(40,0)|]

let bishop = [|(5,0);(5,5);(15,5);(20,15);(10,35);(25,45);(25,50);(30,50);
               (30,45);(45,35);(35,15);(40,5);(50,5);(50,0)|]

let rook = [|(0,0);(0,5);(10,5);(10,30);(5,30);(5,45);(15,45);(15,35);
             (20,35);(20,45);(30,45);(30,35);(35,35);(35,45);(45,45);(45,30);
             (40,30);(40,5);(50,5);(50,0)|]

let knight = [|(5,0); (20,30); (5,25); (5,35); (35,50); (50,30);(50,0)|]


(* Define colors that will be used in the GUI.*)

let color1 = rgb 214 214 179

let color2 = rgb 68 98 157

let c1_shaded = rgb 45 45 45

let c2_shaded = rgb 25 25 25

let yel = rgb 246 200 80

let gry = rgb 230 230 230


(* click_type categorizes user clicks registered by the GUI. *)
type click_type =
  |Square of int*int
  |PromoteRook
  |PromoteKnight
  |PromoteBishop
  |PromoteQueen
  |Ignore

(* [in_list x lst] is true iff x is in lst.*)
let rec in_list x lst =
  match lst with
  |[] -> false
  |h::t -> if h = x then true else in_list x t

(* transformation function used to inverse piece display and correctly
   parse clicks for black player playing on seperate commputer *)
let pos_correction x =
  if Sys.argv.(1)="White" then x else 9-x

(* [draw_squares sq_dim sqs] draws the squares of the chessboard
   onto the GUI window and highlights those squares specified by sqs.
   The size of the squares is determined by sq_dim. *)
let draw_squares sq_dim squares =
  let rec helper row col =
    (if in_list (col,row) squares then
      if (row + col) mod 2 = 0 then set_color c2_shaded else set_color c1_shaded
    else if (row + col) mod 2 = 0 then set_color color2
    else set_color color1);
    let new_row = if col = 8 then row + 1 else row in
    let new_col = if col = 8 then 1 else col + 1 in
    if row = 9 then ()
    else
      (fill_rect (sq_dim * (pos_correction col)) (sq_dim * (pos_correction row)) sq_dim sq_dim; set_color black; set_line_width 1; draw_rect (sq_dim * (pos_correction col)) (sq_dim * (pos_correction row)) sq_dim sq_dim;
       helper new_row new_col)
  in
  helper 1 1

(* inverses display of board labels for black player*)
let conditional_rev l =
  if Sys.argv.(1)="White" then l else List.rev l

(* [label_board] sq_dim puts labels on the GUI window.  Where those labels
   go is determined by sq_dim. *)
let label_board sq_dim =
  let label_column (x,y) s =
    moveto (x + sq_dim/2) y;
    draw_string s;
    set_line_width 3;
    (x + sq_dim, y) in
  let label_row (x,y) s =
    moveto x (y + sq_dim/2);
    draw_string s;
    (x, y + sq_dim) in
  set_color black;
  List.fold_left label_column (sq_dim, sq_dim/2) (conditional_rev ["a";"b";"c";"d";"e";"f";"g";"h"]) |>  ignore;
  List.fold_left label_row (sq_dim/2, sq_dim) (conditional_rev ["1";"2";"3";"4";"5";"6";"7";"8"]) |> ignore

(* [highlight_sq sq_opt sq_dim] highlights square x if sq_opt equals (Some x).
   Otherwise, no square is highlighted.  sq_dim determines the scale of what is drawn.*)
let highlight_sq sq_opt sq_dim =
  match sq_opt with
  |None -> ()
  |Some (c,r) ->
    set_color red;
    set_line_width 4;
    draw_rect (sq_dim * (pos_correction c)) (sq_dim * (pos_correction r)) sq_dim sq_dim

(* [display_pieces sq_dim lst] displays the pieces specified by lst on
   the GUI window.  sq_dim determines where exactly the pieces are displayed. *)
let rec display_pieces sq_dim = function
  |[] -> ()
  |((c,r),(color,piece))::t ->
    (* if color = "White" then set_color red else set_color black; *)
    let f (x,y) = (x + (pos_correction c) * sq_dim, y + (pos_correction r) * sq_dim) in
    let arr = match piece with
      |"King" -> king
      |"Pawn" -> pawn
      |"Queen" -> queen
      |"Bishop" -> bishop
      |"Knight" -> knight
      |"Rook" -> rook
      |_ -> failwith "Impossible" in
    let arr' = Array.map f arr in
    set_color black;
    if color="Black" then fill_poly arr';
    if color="White" then (set_color white; fill_poly arr'; set_color black; set_line_width 2; draw_poly arr');
    display_pieces sq_dim t

(* [display_turn sq_dim str] displays str on the GUI window, with sq_dim
    determining where the string is displayed. *)
let display_turn sq_dim str =
  let x = 11 * sq_dim in
  let y = sq_dim in
  set_color black;
  draw_rect x (y - sq_dim/2) (2 * sq_dim) sq_dim;
  moveto (x + sq_dim/4) y;
  draw_string ("Turn: " ^ str)

(* [display_game_status st sq_dim] displays any pertinent game status messages
   on the GUI window.  sq_dim determines where the messages are displayed. *)
let display_game_status st sq_dim =
  let x = 11 * sq_dim in
  let y = 2 * sq_dim in
  let message = if Board.king_cur_check st then "Check!" else "None" in
  set_color black;
  draw_rect x (y - sq_dim/2) (2 * sq_dim) sq_dim;
  moveto (x + sq_dim/4) y;
  draw_string "Game Status:";
  moveto (x + sq_dim/2) (y - sq_dim/4);
  draw_string message

(* [display_board st sq_opt moves sq_dim] displays many aspects of the game state
   specified by st on the GUI window.  If sq_opt specifies a square, then
   that square is highlighted.  So are the squares in moves.  sq_dim determines
   the scale. *)
let display_board st sq_opt moves sq_dim =
  clear_graph ();
  set_color gry;
  fill_rect 0 0 700 700;
  set_window_title "CS 3110 Final Project: Chess";
  draw_squares sq_dim moves;
  label_board sq_dim;
(*   let img = get_image 0 0 1000 1000 in
  draw_image img 400 300; *)
  let str = Board.get_turn st in
  display_turn sq_dim str;
  display_game_status st sq_dim;
  highlight_sq sq_opt sq_dim;
  let piece_list = Board.get_piece_map st in
  display_pieces sq_dim piece_list

(* [display_promotion_options sq_dim] displays a set of promotion options
   on the GUI window for the user.  The scale is determined by sq_dim. *)
let display_promotion_options sq_dim =
  set_color black;
  set_line_width 5;
  let disp (c,r,str) =
    let x = sq_dim * c in
    let y = sq_dim * r in
    draw_rect x y (3 * sq_dim) sq_dim;
    moveto (x + sq_dim/4) (y + sq_dim/2);
    draw_string str
  in
  List.iter disp [(10,3,"Promote to Knight");
                  (10,4,"Promote to Bishop");
                  (10,5,"Promote to Rook");
                  (10,6,"Promote to Queen")
                 ]


(* [parse_click x y sq_dim] parses a click at position (x,y) relative to
   the left corner of the GUI window into a value of type click_type.  The parsing
   uses sq_dim to correctly interpret the click. *)
let parse_click x y sq_dim =
  let col = x/sq_dim in
  let row = y/sq_dim in
  let b = col >= 10 && col <= 12 in
  if col >= 1 && col <= 8 && row >= 1 && row <= 8 then Square (pos_correction col,pos_correction row)
  else if b && row = 3 then PromoteKnight
  else if b && row = 4 then PromoteBishop
  else if b && row = 5 then PromoteRook
  else if b && row = 6 then PromoteQueen
  else Ignore

(* [(ic,oc)] is the in_channel and out_channel of the connection
 * with the server specified by the ip and port numbers in when the
 * executable was ran. *)
let (ic,oc) = Client.open_con Sys.argv.(2) Sys.argv.(3)


(* [get_sq_dim ()] returns an int proportional to the length of the smaller
   dimension of the GUI window. *)
let get_sq_dim () =
  let window_width = size_x () in
  let window_height = size_y () in
  let scale = min window_width window_height in
  scale/13

(* [game_loop st (click_status, sq_opt, squares, mv_lst)] executes
   the main event loop for the GUI. *)
let rec game_loop st (click_status, sq_opt, squares, mv_lst) =
  try
    let sq_dim  = get_sq_dim () in
    display_board st sq_opt squares sq_dim;
    if Sys.argv.(1) = Board.get_turn st then
      let s = wait_next_event [Button_down] in
(*       let () = sound 1500 1000 in *)
      let click_type = parse_click s.mouse_x s.mouse_y sq_dim in
      if click_status then
        match click_type, sq_opt with
        |Square (c,r), Some (c',r') ->
          begin
            match Board.get_corresponding_mv_pc (c',r') (c,r) mv_lst st with
            |Some mv, Some piece ->
              if Board.is_PromotionMove mv
              then (
                display_promotion_options sq_dim;
                let s' = wait_next_event [Button_down] in
                let click_type' = parse_click s'.mouse_x s'.mouse_y sq_dim in
                let (st',mv'),flag =
                  match click_type' with
                  |PromoteKnight -> (Board.move st mv piece (Some "K")),true
                  |PromoteBishop -> (Board.move st mv piece (Some "B")),true
                  |PromoteRook -> (Board.move st mv piece (Some "R")),true
                  |PromoteQueen -> (Board.move st mv piece (Some "Q")),true
                  |_ -> (st,mv),false
                in
                if flag then
                  (let mes = ((mv',Board.get_turn st')|>Board.encode) ^ "\n" in
                   output_string oc mes; flush oc; game_loop st' (false, None, [], []))
                else game_loop st (false, None, [], []))
              else
                let () = sound 800 1000 in
                let st',mv' = Board.move st mv piece None in
                let mes = ((mv', Board.opposite_turn st')|> Board.encode)^ "\n" in
                output_string oc mes; flush oc; game_loop st' (false, None, [], [])
            | _ -> game_loop st (false, None, [], [])
          end
        |_ ->
          let () = sound 800 1000 in game_loop st (false, None , [], [])
      else
        match click_type with
        |Square (c,r) ->
          if (Board.is_empty (c,r) st) then let () = sound 800 1000 in game_loop st (false, None, [], [])
          else
            let mv_lst = Board.get_valid_moves st (c,r) in
            let squares = Board.get_valid_squares mv_lst st in
            game_loop st (true, Some (c,r), squares, mv_lst)
        |_ ->
          let () = sound 800 1000 in
          game_loop st (false, None, [], [])
    else
      let opponent_move_str = input_line ic in
      let opp_move = Board.decode opponent_move_str st in
      let st',mv' = Board.move st (opp_move|>snd|>fst) (opp_move|>fst|>Board.piece_from_op) (opp_move|>snd|>snd) in
      game_loop st' (false, None, [], [])
  with
  |Graphic_failure _ -> ()
  |_ -> () |> get_sq_dim |> display_board st sq_opt squares


(* Open the GUI window and launch the game loop. *)
let _ =
  open_graph " 700x700";
  game_loop (Board.init_state ()) (false, None, [], [])
