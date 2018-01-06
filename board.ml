type position = int * int

type piece_type =
  | King | Queen | Rook | Bishop | Knight | Pawn

type piece_color = Black | White

type move =
  | Normal of position * position
  | Castle_Left | Castle_Right
  | EnPassant of position * position
  | Pre_Promotion of position * position
  | Full_Promotion of piece_type * position * position

type direction = (int * int) list

type piece = {
  color : piece_color;
  piece_type : piece_type;
  piece_value : int;
  piece_direction : direction;
  last_move : move option;
}

type command = piece_type * move

type board = (position, piece option) Hashtbl.t

type turn = piece_color

(* TODO: Add more *)
type state = {
  turn : turn;
  white_king_pos : position;
  black_king_pos : position;
  board : board;
  enPassant_target : position option;
}

let make_state b c d e f =
  {
    turn = b;
    white_king_pos = c;
    black_king_pos = d;
    board = e;
    enPassant_target = f;
  }

(* Helper function to get a [piece option] from state. *)
let get_piece state position =
  Hashtbl.find state.board position

let get_color piece = piece.color

let get_turn_color st = st.turn

(* [piece_from_op opt] is the pieces wrapped in an an option *)
let piece_from_op = function
  | Some x -> x
  | None -> failwith "Not a piece on this square"

(*TODO: make getter function of state for william:
    get_piece_mappings: (int*int)*(string*string)list
*)

let make_pos (x : int) (y : int) : position = (x, y)

let get_value piece_type =
  match piece_type with
  | King   -> 10000
  | Queen  -> 900
  | Rook   -> 500
  | Bishop -> 300
  | Knight -> 300
  | Pawn   -> 100

let get_board st =
  st.board

(* Directions of pieces. *)
let rook_dir = [(1,0); (-1,0); (0,1); (0,-1)]
let knight_dir = [(1,2); (-1,2); (2,1); (-2,1); (1,-2); (-1,-2); (2,-1); (-2,-1)]
let bishop_dir = [(1,1); (1,-1); (-1,1); (-1,-1)]
let queen_dir = rook_dir @ bishop_dir
let king_dir = queen_dir
let white_pawn_move_dir = [(0,1)]
let white_pawn_cap_dir  = [(1,1); (-1,1)]
let black_pawn_move_dir = [(0,-1)]
let black_pawn_cap_dir  = [(1,-1); (-1,-1)]

exception Invalid_Move

let get_dir piece_type color =
  match piece_type with
  | Rook -> rook_dir
  | Knight -> knight_dir
  | Bishop -> bishop_dir
  | Queen -> queen_dir
  | King -> king_dir
  | Pawn -> if color = White then white_pawn_move_dir else black_pawn_move_dir

let make_piece ?last_move:(arg=None) piece_type color =
  {
  color = color;
  piece_type = piece_type;
  piece_value = get_value piece_type;
  piece_direction = get_dir piece_type color;
  last_move = arg;
}

(*
   Returns:  EnPassant wrapped move
   Requires: Valid EnPassant move
*)
let construct_en_passant (init_pos:position) (end_pos:position) :move=
  EnPassant (init_pos, end_pos)

(*
   Returns:
   promotion of pawn, including piece type pawn is promoted to
   Requires:
   white pawn:is on the 8th row and promotion piece chosen
   black pawn: is on first row and promotion piece chosen
*)
let construct_full_promotion (init_pos:position) (end_pos:position) (new_piece:piece_type):move=
  Full_Promotion (new_piece, init_pos,end_pos)


let construct_std_move (pees_type:piece_type) (init_pos:position) (end_pos:position):move=
  match pees_type with
  |King when init_pos=(5,1)&&end_pos=(3,1)-> Castle_Left
  |King when init_pos=(5,8)&&end_pos=(3,8)-> Castle_Left
  |King when init_pos=(5,1)&&end_pos=(7,1)-> Castle_Right
  |King when init_pos=(5,8)&&end_pos=(7,8)-> Castle_Right
  |Pawn when (snd init_pos)=7&& (snd end_pos)=8-> Pre_Promotion (init_pos, end_pos)
  |Pawn when (snd init_pos)=2&& (snd end_pos)=1-> Pre_Promotion (init_pos, end_pos)
  |King |Queen |Rook |Bishop |Knight |Pawn -> Normal (init_pos, end_pos)


(* Initialize [board], which is (piece, position) hash table. *)
let init_board () =
  (* Initialize empty board *)
  let rec init_hashtbl x y board =
    if x = 8 && y = 8
    then (Hashtbl.add board (make_pos x y) None; board)
    else if y < 8
    then (Hashtbl.add board (make_pos x y) None; init_hashtbl x (y+1) board)
    else (Hashtbl.add board (make_pos x y) None; init_hashtbl (x+1) 1 board)
  in

  (* Place pieces on the board *)
  let board = init_hashtbl 1 1 (Hashtbl.create 64) in
  let formation = [Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook] in
  let rec init_pieces formation board index =
    match formation with
    | [] -> ()
    | h::t ->
    begin
      Hashtbl.add board (make_pos index 1) (Some (make_piece h White));
      Hashtbl.add board (make_pos index 2) (Some (make_piece Pawn White));
      Hashtbl.add board (make_pos index 8) (Some (make_piece h Black));
      Hashtbl.add board (make_pos index 7) (Some (make_piece Pawn Black));
      init_pieces t board (index+1)
    end
  in init_pieces formation board 1; board



(* Initialize [state]. *)
let init_state () = {
  turn = White;
  white_king_pos = make_pos 5 1;
  black_king_pos = make_pos 5 8;
  board = init_board ();
  enPassant_target = None;
}

(* ---------------- Simple and useful helper functions. ------------------ *)
let tuple_add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

let tuple_subtract (x1,y1) (x2,y2) = (x1-x2, y1-y2)

let tuple_mult a (x,y) = (a*x, a*y)

let take_off_option x =
  match x with
  | Some x -> x
  | _ -> failwith "None cannot take off option"

(* Checks if (x,y) is within the board. *)
let within_board (x,y) =
  if (x > 8 || 1 > x || y > 8 || 1 > y) then false else true



(*------------------------------Updates Board---------------------------------*)

(*
  move a given piece to desired location on board.
  for the purposes of AI, it has potential to consider points of capture****IMPORTANT*******
*)
let apply_move (pot_move:move) (moved_piece:piece) (board_before: board) :board =
  match pot_move with
    |Normal (p1,p2)->
        let new_piece = make_piece ~last_move:(Some pot_move) moved_piece.piece_type moved_piece.color in

        Hashtbl.replace board_before p1 None;
        Hashtbl.replace board_before p2 (Some new_piece);
        board_before

    |Castle_Left when moved_piece.color=White ->
        let new_piece = make_piece ~last_move:(Some pot_move) King White in
        let rook_move = Normal ((1,1),(4,1)) in
        let new_rook = make_piece ~last_move:(Some rook_move) Rook White in

        Hashtbl.replace board_before (5,1) None;
        Hashtbl.replace board_before (3,1) (Some new_piece);
        Hashtbl.replace board_before (4,1) (Some new_rook);
        Hashtbl.replace board_before (1,1) None;
        board_before

    |Castle_Left when moved_piece.color=Black ->
        let new_piece = make_piece ~last_move:(Some pot_move) King Black in
        let rook_move = Normal ((1,8),(4,8)) in
        let new_rook = make_piece ~last_move:(Some rook_move) Rook Black in

        Hashtbl.replace board_before (5,8) None;
        Hashtbl.replace board_before (3,8) (Some new_piece);
        Hashtbl.replace board_before (4,8) (Some new_rook);
        Hashtbl.replace board_before (1,8) None;
        board_before

    |Castle_Right when moved_piece.color=White ->
        let new_piece = make_piece ~last_move:(Some pot_move) King White in
        let rook_move = Normal ((8,1),(6,1)) in
        let new_rook = make_piece ~last_move:(Some rook_move) Rook White in

        Hashtbl.replace board_before (5,1) None;
        Hashtbl.replace board_before (7,1) (Some new_piece);
        Hashtbl.replace board_before (6,1) (Some new_rook);
        Hashtbl.replace board_before (8,1) None;
        board_before

    |Castle_Right when moved_piece.color=Black ->
        let new_piece = make_piece ~last_move:(Some pot_move) King Black in
        let rook_move = Normal ((8,8),(6,8)) in
        let new_rook = make_piece ~last_move:(Some rook_move) Rook Black in

        Hashtbl.replace board_before (5,8) None;
        Hashtbl.replace board_before (7,8) (Some new_piece);
        Hashtbl.replace board_before (6,8) (Some new_rook);
        Hashtbl.replace board_before (8,8) None;
        board_before

    |Pre_Promotion (p1,p2)->
        Hashtbl.replace board_before p1 None;
        Hashtbl.replace board_before p2 (Some moved_piece);
        board_before;

    |Full_Promotion (pees,pos_b,pos_a) ->
        let new_piece = make_piece ~last_move:(Some pot_move) pees moved_piece.color in

        Hashtbl.replace board_before pos_b None;
        Hashtbl.replace board_before pos_a (Some new_piece);
        board_before

(*     |EnPassant ((x1,y1),(x2,y2)) when ((y2-y1=1)||(y2-y1= ~-1)) &&(x2-x1= ~-1) -> *)
    | EnPassant ((x1,y1),(x2,y2)) ->
        let new_piece = make_piece ~last_move:(Some pot_move) Pawn moved_piece.color in

        Hashtbl.replace board_before (x1,y1) None;
        Hashtbl.replace board_before (x2,y2) (Some new_piece);
        Hashtbl.replace board_before (x2,y1) None;
        board_before

    |_-> raise Invalid_Move


(* ----------------------------------------------------------------------- *)


(*------------------------Determing if king is in Check-----------------------*)


(*
  Returns:
  true if there is an enemy rook, queen, or bishop attacking from afar in a given
  direction, false if there is not.
  HELPER
  Requries:
  linear direction dir,start position, enemy piece_color, and the board
*)
let rec enemy_at_lin (start_pos:position) (self_col:piece_color) (brd:board) (dir: int*int):bool=
  let next_pos = tuple_add start_pos dir in
  if not( within_board next_pos) then false else
    match Hashtbl.find brd next_pos with
    |None-> enemy_at_lin  next_pos self_col brd dir
    |Some p-> if p.color=self_col then false else
        match p.piece_type with
        |Rook   when (dir=(0,1)||dir=(0,-1)||dir=(1,0) ||dir=(-1,0)) ->true
        |Queen  when (dir=(0,1)||dir=(0,-1)||dir=(1,0) ||dir=(-1,0)) ->true
        |Queen  when (dir=(1,1)||dir=(1,-1)||dir=(-1,1)||dir=(-1,-1))->true
        |Bishop when (dir=(1,1)||dir=(1,-1)||dir=(-1,1)||dir=(-1,-1))->true
        |_ -> false

(* Returns:
   true if there's an enemy queen, rook or bishop attacking from afar
   HELPER to determine if king is under check
*)
let lin_check (start_pos:position) (self_col:piece_color) (brd:board) =
  match (List.filter (enemy_at_lin start_pos self_col brd) queen_dir) with
  |[]-> false
  |_ -> true

(* Returns:
   true if a pawn is attacking a particular square, start_pos,
   given a particular direction, dir
   HELPER for determing if any pawns are attacking a king
*)
let pawn_attacking_help (start_pos:position) (self_col:piece_color) (brd:board) (dir:int*int):bool=
  let next_pos = tuple_add start_pos dir in
  if not( within_board next_pos) then false else
    match Hashtbl.find brd next_pos with
    |None-> false
    |Some p-> if p.color<>self_col && p.piece_type=Pawn then true else false


(* Returns:
   true if there are any pawns are attacking a particular square, start pos
   used as helper function to determine if king is undercheck by any possible pawns
*)
let pawn_attacking (start_pos:position) (self_col:piece_color) (brd:board):bool=
  match (List.filter (pawn_attacking_help start_pos self_col brd)  (black_pawn_cap_dir@white_pawn_cap_dir)) with
  |[]->false
  |_::_ -> true

(* Returns:
   True if there is a knight attacking a square, start_pos, from a particlar direction, dir
   Used as helper to determine if any knights are attacking start_pos
*)
let knight_attacking_help (start_pos:position) (self_col:piece_color) (brd:board) (dir:int*int):bool=
  let next_pos = tuple_add start_pos dir in
  if not( within_board next_pos) then false else
    match Hashtbl.find brd next_pos with
    |None-> false
    |Some p-> if p.color<>self_col && p.piece_type=Knight then true else false

(*
  Returns:
  true if an enemy knight is attacking king that is in start_pos.
  used as helper function to determine if king is under check
*)
let knight_attacking (start_pos:position) (self_col:piece_color) (brd:board):bool=
  match (List.filter (knight_attacking_help start_pos self_col brd)  knight_dir) with
  |[]->false
  |_::_ -> true


(* Returns:
   Position of king after applying a moved
   Can be used when determing check, as well as to update state
*)
let king_pos_after_move (cur_state:state)  (da_move:move) (da_piece:piece) :int*int=
  match da_piece.piece_type with
  |King->
    begin
      match da_move with
      |Normal (_,y)-> y
      |Castle_Left  when da_piece.color=White -> (3,1)
      |Castle_Left  when da_piece.color=Black -> (3,8)
      |Castle_Right when da_piece.color=White -> (7,1)
      |Castle_Right when da_piece.color=Black -> (7,8)
      |_ -> failwith "not valid move"
    end
  | Queen | Rook | Bishop | Knight | Pawn->
    if da_piece.color=White then cur_state.white_king_pos
    else cur_state.black_king_pos


(* Given a physically possible move,we check if the move puts the king in check
   A player uses it to see if they are under check after executing their move *)
let king_checked_after_move (cur_state:state)  (da_move:move) (da_piece:piece) :bool =
  let board_copy = (Hashtbl.copy cur_state.board )|> apply_move da_move da_piece  in
  let cur_king_pos = king_pos_after_move cur_state da_move da_piece in
  (
  knight_attacking cur_king_pos da_piece.color board_copy||
  pawn_attacking   cur_king_pos da_piece.color board_copy||
  lin_check        cur_king_pos da_piece.color board_copy
  )

let king_cur_check (cur_state:state) :bool =
  let cur_board = cur_state.board  in
  let cur_king_pos,king_col = match cur_state.turn with
    |White-> cur_state.white_king_pos,White
    |Black-> cur_state.black_king_pos,Black
  in
  (
  knight_attacking cur_king_pos king_col cur_board||
  pawn_attacking   cur_king_pos king_col cur_board||
  lin_check        cur_king_pos king_col cur_board
  )


let king_safe_after_move (cur_state:state) (da_piece:piece) (da_move:move):bool =
  not (king_checked_after_move cur_state  da_move da_piece)


(* For [Rook], [Bishop], [Queen], get all possible squares along [dir]. *)
let rec get_all_along_dir current dir color board (squares : position list) =
  let next = tuple_add current dir in
  if within_board next then
    let next_piece = Hashtbl.find board next in
    if next_piece = None
    then get_all_along_dir next dir color board (next::squares)
    else if (take_off_option next_piece).color = color
    then squares
    else (next::squares)
  else
    squares

let real_dir (x,y) =
  let rx = if x>1 then 1 else if x< -1 then -1 else x in
  let ry = if y>1 then 1 else if y< -1 then -1 else y in
  (rx,ry)

(* [is_empty_between pos1 pos2 board] is [true] if squares between
 * [from] and [to] are empty. [false] otherwise. It searches from [from] to [to]
 * through [dir] direction.
 * Requires: [to] has to be reachable from [from] by moving through [dir] direction.
   i.e., (3,0) is reachable from (1,0) in (1,0) direction. *)
let rec is_empty_between (pos_from:position) (pos_to:position) (dir:int*int) board =
  let next = tuple_add pos_from (real_dir dir) in
  if (within_board next)
  then
    let next_piece = Hashtbl.find board next in (* By RI, no need to check boundary. *)
    if  (next = pos_to) then true
    else if (next_piece <> None) then false
    else is_empty_between next pos_to dir board
  else
    true

let is_jumping (x,y) = (x = 2 || x= -2 || y = 2 || y = -2) && (x <> 0)


let habitable_square (pos:position) (brd:board) (self_col:piece_color) =
  if not( within_board pos) then false else
  match (Hashtbl.find brd pos) with
  |None  -> true
  |Some p-> if (p.color <>self_col) then true else false

let rec get_habitable_inline (pos:position) (brd:board) (self_col:piece_color) (dir:int*int)=
  let next_pos = tuple_add pos dir in
  if  not (within_board next_pos) then [] else
    match (Hashtbl.find brd next_pos) with
    |None  -> next_pos::(get_habitable_inline next_pos brd self_col dir)
    |Some p->
      if p.color=self_col||p.piece_type=King then []
      else next_pos::(get_habitable_inline next_pos brd self_col dir)

let habitable_step (pos:position) (brd:board) (self_col:piece_color) (dir:int*int)=
  let next_pos = tuple_add pos dir in
  if not (within_board next_pos) then [] else
    match (Hashtbl.find brd next_pos) with
    |None  -> next_pos::(get_habitable_inline next_pos brd self_col dir)
    |Some p->
      if p.color=self_col||p.piece_type=King then []
      else next_pos::(get_habitable_inline next_pos brd self_col dir)


(* Helper function that returns all possible squares from [dir_list].
   If [is_linear] is [true], that is, if the piece is linearly-moving piece
   (i.e., [Bishop], [Rook], etc.) then [get_all_along_dir] function is used.
   Otherwise, for pieces like [Pawn], [Knight] and [King], only the next squares
   are counted. *)
let rec get_possible ?is_pawn:(arg=false) position color dir_list board (is_linear : bool) =
  if is_linear then
    match dir_list with
      | [] -> []
      | dir::t -> ((get_all_along_dir position dir color board []) @ (get_possible position color t board true))
  else
    match dir_list with
    | [] -> []
    | dir::t ->
      let pos = tuple_add position dir in (* Get next square. *)
      if (within_board pos) then (* See if next square is empty. *)
        let pos_piece = Hashtbl.find board pos in
        if is_jumping dir then
          if ((pos_piece = None) || (take_off_option pos_piece).color <> color)
          then pos::(get_possible position color t board false)
          else get_possible position color t board false
        else
          if (arg) then
            if ((is_empty_between position pos dir board) && (pos_piece = None)) then pos::(get_possible position color t board false)
            else get_possible position color t board false
          else
            if ((is_empty_between position pos dir board) && (pos_piece = None) ||
               (is_empty_between position pos dir board) && (take_off_option pos_piece).color <> color)
            then pos::(get_possible position color t board false)
            else get_possible position color t board false

      else get_possible position color t board false

(* Check if the current position is at enpassant-able position (row).
   En Passant is only possible on 4th or 5th row, depending on the [color].
   TODO: Remove if not used (enpassant is being tracked now). *)
let can_enPanssant (_,y) color =
  if ((color = White && y = 5) || (color = Black && y = 4)) then true else false


(* Helper function that counts possible moves (squares) possible by capturing of pawn. *)
let rec get_cap_moves position color cap_dir board =
  match cap_dir with
  | [] -> []
  | dir::t ->
    let pos = tuple_add position dir in
    if (within_board pos) then
      let pos_piece = Hashtbl.find board pos in
      if (pos_piece <> None && (take_off_option pos_piece).color <> color) then
      pos::(get_cap_moves position color t board)
      else get_cap_moves position color t board
    else get_cap_moves position color t board

(* 2. Check En-Passant.
 *  IMPORTANT: The set of En-Passant moves and that of capturing are disjoint.
    i.e., never happen both at the same time on one side.
    That means we can concatenate the lists and the result will still be
    unique-element list. *)
let rec get_enPassant_moves position color enPassant_dir target_pos board =
  match enPassant_dir with
  | [] -> []
  | dir::t ->
    let pos = tuple_add position dir in
    if pos = target_pos then
      let pos_to = if color = White then tuple_add pos (0,1) else tuple_add pos (0,-1) in pos_to::[]
    else get_enPassant_moves position color t target_pos board

(* Returns all possible diagonal moves of [Pawn] by capturing or/and En Passant. *)
let get_possible_diagonal_moves position color enPassant_target board =

  (* 1. Check capturables. *)
  let cap_dir = if color = White then white_pawn_cap_dir else black_pawn_cap_dir in
  let cap_possible_moves = get_cap_moves position color cap_dir board in

  let enPassant_moves =
    if enPassant_target <> None then (* Check if there is a target. *)
      let target_pos = take_off_option enPassant_target in
      get_enPassant_moves position color [(1,0);(-1,0)] target_pos board
    else []
  in cap_possible_moves, enPassant_moves

(* Returns list size of up to 2 of additional possible positions by castling.
   If there is no chance of castling, returns empty list.  *)
let get_moves_by_castling piece st : position list =

  let brd=st.board in
  (* First, check if [King] has moved. *)
  let has_moved = piece.last_move <> None in

  (* Then, check left and right [Rook]. [l/r/k_pos] are left/right [Rook]
     and [King]'s positions respectively. [l/r_pos_to] are positions for [King]
     to move if conditions are satisfied. *)
  let left_Rook, right_Rook, l_pos, r_pos, k_pos, l_pos_to, r_pos_to =
    if piece.color = White then
      let l_pos = make_pos 1 1 in
      let r_pos = make_pos 8 1 in
      let k_pos = make_pos 5 1 in
      let l_pos_to = make_pos 3 1 in
      let r_pos_to = make_pos 7 1 in
      (Hashtbl.find brd l_pos), (Hashtbl.find brd r_pos), l_pos, r_pos, k_pos, l_pos_to, r_pos_to
    else
      let l_pos = make_pos 1 8 in
      let r_pos = make_pos 8 8 in
      let k_pos = make_pos 5 8 in
      let l_pos_to = make_pos 3 8 in
      let r_pos_to = make_pos 7 8 in
      (Hashtbl.find brd l_pos), (Hashtbl.find brd r_pos), l_pos, r_pos, k_pos, l_pos_to, r_pos_to in

  let lR_possible = (left_Rook  <> None) && ((take_off_option left_Rook).last_move  = None) in
  let rR_possible = (right_Rook <> None) && ((take_off_option right_Rook).last_move = None) in

  let l_clear = (not has_moved) && lR_possible && (is_empty_between k_pos l_pos (-1,0) brd) in
  let r_clear = (not has_moved) && rR_possible && (is_empty_between k_pos r_pos (1, 0) brd)  in

  let l_safe = king_safe_after_move st piece (Normal (k_pos,tuple_add k_pos (-1,0))) in
  let r_safe = king_safe_after_move st piece (Normal (k_pos,tuple_add k_pos (1, 0))) in

  let l_square = if (l_clear && l_safe) then l_pos_to::[] else [] in
  let r_square = if (r_clear && r_safe) then r_pos_to::[] else [] in

  (l_square @ r_square)


(* ---------------------------------------------------------------------------- *)

(* Returns list of possible move in [move list].
   For [Pawn] and [King], special routines are applied (En Passant, Capturing, etc.)
   Requires: [within_board position] is [true].
   IMPORTANT: When piece is [King], squares that make [King] die should be
              eliminated outside this function (does not take that account here).  *)
let get_possible_moves position state =
  let board = state.board in
  let piece_opt = Hashtbl.find board position in
  if piece_opt = None then []
  else
    let piece = take_off_option piece_opt in
    if piece.color = state.turn then
      match piece.piece_type with
      | Rook | Bishop | Queen ->
        let dir_list = piece.piece_direction in
        let color = piece.color in
        List.map (construct_std_move piece.piece_type position) (get_possible position color dir_list board true)

      | Knight ->
        let dir_list = piece.piece_direction in
        let color = piece.color in
        List.map (construct_std_move piece.piece_type position) (get_possible position color dir_list board false)

      | Pawn ->
        let dir_list = piece.piece_direction in
        let color = piece.color in
        let has_moved = piece.last_move <> None in

        (* 1. Get normal possible moves. *)
        let normal_possible_moves =
          if has_moved then List.map (construct_std_move piece.piece_type position) (get_possible ~is_pawn:true position color dir_list board false)
          else
          let opening_move_dir = tuple_mult 2 (List.hd dir_list) in (* for opening, [Pawn] can move twice more. *)
          List.map (construct_std_move piece.piece_type position) (get_possible ~is_pawn:true position color (opening_move_dir::dir_list) board false)
        in

        (* 2. Get special possible moves by capturing and En Passant. *)
        let enPassant_target = state.enPassant_target in
        let cap_moves, en_moves = get_possible_diagonal_moves position color enPassant_target board in

        ((normal_possible_moves @ (List.map (construct_std_move piece.piece_type position) cap_moves ))@ (List.map (construct_en_passant position) en_moves))

      | King ->
        let dir_list = piece.piece_direction in
        let color = piece.color in

        (* 1. Get normal possible positions. *)
        let normal_possible_moves = (get_possible position color dir_list board false) in

        (* 2. Get special possible positions by castling. *)
        let special_possible_moves = if king_cur_check state then [] else get_moves_by_castling piece state in
        List.map (construct_std_move piece.piece_type position) (normal_possible_moves @ special_possible_moves)
    else []


let get_valid_moves (cur_state:state) (cur_pos:position) :move list=
  List.filter (king_safe_after_move cur_state (take_off_option (Hashtbl.find cur_state.board cur_pos))) (get_possible_moves cur_pos cur_state)

(*-----------------------------------------------------------------------------*)
let hashtbl_to_assoc (tbl)=
  Hashtbl.fold (fun k v acc->(k,v)::acc) tbl []


let is_checkmate (cur_state):bool =

  let rec has_moves st mappings =
  match mappings with
    |[]-> false
    |(_,p_opt )::t   when p_opt=None-> has_moves st t
    |(_,Some p)::t   when p.color !=st.turn ->has_moves st t
    |(pos,Some p)::t when p.color  =st.turn -> begin
      match get_valid_moves st pos with
      |_::_-> true
      |_->has_moves st t end
    |_-> failwith "Wrong mappings argument" in
  has_moves cur_state (cur_state.board|>hashtbl_to_assoc )




(*----------------------------------------------------------------------------*)


(*----------------------------Interfacing with GUI----------------------------*)
(* Returns:
   an association list with bindings specified in hashtabl.
   meant as helper function for input into GUI but can always be used
   Requires:
   A hashtabl
*)




(* Returns:
   Changes format of key value pair from position*piece to (int*int)*(string*string)
   for formating association list for GUI
   HELPER
*)
let format_tup kv =
  match kv with
  |((x,y),None)-> ((x,y),("",""))
  |((x,y),Some p)->
    match p.piece_type with
    |King   when p.color=White-> ((x,y),("White","King"))
    |King   when p.color=Black-> ((x,y),("Black","King"))
    |Queen  when p.color=White-> ((x,y),("White","Queen"))
    |Queen  when p.color=Black-> ((x,y),("Black","Queen"))
    |Rook   when p.color=White-> ((x,y),("White","Rook"))
    |Rook   when p.color=Black-> ((x,y),("Black","Rook"))
    |Bishop when p.color=White->((x,y),("White","Bishop"))
    |Bishop when p.color=Black->((x,y),("Black","Bishop"))
    |Knight when p.color=White->((x,y),("White","Knight"))
    |Knight when p.color=Black->((x,y),("Black","Knight"))
    |Pawn   when p.color=White-> ((x,y),("White","Pawn"))
    |Pawn   when p.color=Black-> ((x,y),("Black","Pawn"))
    |_-> failwith "invalid mapping"

let format_assoc ass =
  List.map format_tup ass|> List.filter (fun x->snd x<>("",""))

(* Returns:
   square to piece mapping formatted for GUI
   Requires:
   current state of match
*)
let get_piece_map (st:state) =
  st.board |>hashtbl_to_assoc|>format_assoc


(*
  Returns:
  true if a move is a promotion
  Requires:
  current state, start position, end position
*)
let is_promotion_move (st:state) (pos1:int*int) (pos2:int*int) :bool=
  if not ((snd pos1=7&&snd pos2=8)||(snd pos1=2&&snd pos2=1)) then false else
  match Hashtbl.find st.board pos1 with
  |None-> false
  |Some p->
    match p.piece_type with
    |Pawn -> true
    |_    -> false


let move_present (pos2:position)  (move:move)=
  match move with
  |Normal (_,p2) |EnPassant (_,p2) |Pre_Promotion (_,p2)->
    if p2=pos2 then true else false
  |Castle_Left  when pos2=(3,1)||pos2=(3,8)->true
  |Castle_Right when pos2=(7,1)||pos2=(7,8)->true
  |_-> false

let rec get_chosen_move_and_piece (pos1:position) (pos2:position) (move_list: move list) (st:state)=
  match move_list with
  |[]-> raise Invalid_Move
  |h::t when not((move_present pos2 h))->get_chosen_move_and_piece pos1 pos2 t st
  |h::_ when move_present pos2 h-> (h,get_piece st pos1)
  |_-> raise Invalid_Move






(*--------------------------------Execution----------------------------------*)
(* Before execute,
    1. verify if the clicked position is valid move, i.e., from the possible moves we displayed.
    2. If the position is Pre-Promotion then prompt option to promote and wait for 3rd click and
       convert to Full_promotion. *)


(* Helper function that returns if King's move changes or Pawn jumped. *)
let check_piece piece_type move state turn =
  match piece_type, move with
  | King, Castle_Left when turn = White -> Some (3,1)
  | King, Castle_Left when turn = Black -> Some (3,8)
  | King, Castle_Right when turn = White -> Some (7,1)
  | King, Castle_Right when turn = Black -> Some (7,8)
  | King, Normal (pos1, pos2) -> Some pos2

  | Pawn, Normal (pos1, pos2) when turn = White ->
    if tuple_subtract pos2 pos1 = (0,2) then Some pos2
    else None
  | Pawn, Normal (pos1, pos2) when turn = Black ->
    if tuple_subtract pos2 pos1 = (0,-2) then Some pos2
    else None

  | _, _ -> None

let get_next (turn:piece_color) =
  if turn = White then Black else White

(* 1. When executing, update the field enpassantable.
   2. Update turn and board
   Require: the position provided is valid move, from the possible moves provided before.
   Before using [execute], we need to first know what piece that is, and
   search [move] along the possible move list,
   that correspond the position that was clicked by user.
   IMPORTANT: The turn has to be agreed with actual turn !!! (each side can only do execute once at a time)  *)
let execute state move piece =
  let board = state.board in
  let new_board = apply_move move piece board in
  let wk_pos = state.white_king_pos in
  let bk_pos = state.black_king_pos in
  let piece_type = piece.piece_type in
  let turn = state.turn in

  let new_pos = check_piece piece_type move state turn in
  match new_pos, piece_type with
  | Some pos, King when turn=White -> make_state (get_next turn) pos bk_pos new_board None
  | Some pos, King when turn=Black -> make_state (get_next turn) wk_pos pos new_board None
  | Some pos, Pawn -> make_state (get_next turn) wk_pos bk_pos new_board new_pos
  | _ -> make_state (get_next turn) wk_pos bk_pos new_board None

(*----------------------------------SIGNAL------------------------------------*)



(*----------------------------------Encode -----------------------------------*)
let pos_to_str (x,y : position) = (string_of_int x) ^ (string_of_int y)

let piece_type_to_str piece_type =
  match piece_type with
  | Rook   -> "R"
  | Knight -> "N"
  | Bishop -> "B"
  | Queen  -> "Q"
  | Pawn   -> "P"
  | King   -> "K"

let move_to_str move =
  match move with
  | Normal (x,y)      -> "NO/" ^ (pos_to_str x) ^ "/" ^ (pos_to_str y)
  | Castle_Left     -> "CL"
  | Castle_Right    -> "CR"
  | EnPassant (x,y)   -> "EP/" ^ (pos_to_str x) ^ "/" ^ (pos_to_str y)
  | Full_Promotion (p,x,y) -> "PR/" ^ (pos_to_str x) ^ "/" ^ (pos_to_str y) ^ "/" ^ (piece_type_to_str p)
  | _ -> failwith "invalid"

(* We in fact do not need to have valid FEN encoding method. We assume both
   clients will have the same [state]. Execution is only allowed when its user's
   turn, so there will not be any concurrency issue. Therefore, we may send
   minimal information: piece type, move type, move from, move to.  *)
let encode (move, str) =
  str ^ "/" ^ (* (piece_type_to_str piece_type) ^ "/" ^*) (move_to_str move)


(*------------------------------- Decode -------------------------------------*)
let str_to_piece_type str =
  match str with
  | "R" -> Rook
  | "K" -> King
  | "N" -> Knight
  | "Q" -> Queen
  | "P" -> Pawn
  | "B" -> Bishop
  | _   -> failwith str

let str_to_pos (str : string) : position =
  let x = String.get str 0 in
  let y = String.get str 1 in
  (int_of_char x) - 48, (int_of_char y) - 48

(* Convert string message to [command]. *)
let decode (str :string) st =
  let code = String.split_on_char '/' str in
  let color = List.hd code in
  (*let piece_type = str_to_piece_type (List.nth code 1) in*)
  let move_type = List.nth code 1 in
  let get_pos x = (x)|>List.nth code|>str_to_pos in

  let make_move move_type code =
    match move_type with
    | "NO" ->
      let piece = 2|>get_pos|>get_piece st in
      piece,(Normal (get_pos 2, get_pos 3),None)
    | "CL" ->
      let pos =
      match color with
        | "White" -> make_pos 5 1
        | _ -> make_pos 5 8
      in
        let piece = pos|>get_piece st in
        piece,(Castle_Left,None)
    | "CR" ->
      let pos =
        match color with
        | "White" -> make_pos 5 1
        | _ -> make_pos 5 8
      in
      let piece = pos|>get_piece st in
      piece,(Castle_Right,None)
    | "EP" ->
      let piece = 2|>get_pos|>get_piece st in
      piece,(EnPassant (get_pos 2, get_pos 3),None)
    | "PR" ->
      let piece_str = List.nth code 4 in
      let piece = 2|>get_pos|>get_piece st in
      piece,(Full_Promotion (piece_str|>str_to_piece_type, get_pos 2, get_pos 3),Some piece_str)
    | _    -> failwith "Invalid Message"
  in make_move move_type code


(*--------------------------------- For GUI ------------------------------------*)

let helper_move move state =
  match move with
  | Normal (x,y) -> y
  | Castle_Left when state.turn=White -> (3,1)
  | Castle_Left when state.turn=Black -> (3,8)
  | Castle_Right when state.turn=White -> (7,1)
  | Castle_Right when state.turn=Black -> (7,8)
  | EnPassant (x,y) -> y
  | Pre_Promotion (x,y) -> y
  | _ -> failwith "Invalid move to convert in this step"

let rec convert_moves_to_squares mv_lst state =
  match mv_lst with
  | [] -> []
  | move::t -> (helper_move move state)::(convert_moves_to_squares t state)

let get_valid_squares mv_lst st =
  convert_moves_to_squares mv_lst st

let is_PromotionMove mv =
  match mv with
  | Pre_Promotion _ -> true
  | _ -> false

let move state move (piece:piece) pc_option =
  if pc_option = None then (execute state move piece), move
  else
    let type_str = take_off_option pc_option in
    let pc_type = str_to_piece_type type_str in
    let get_full_promotion move pc_type =
      match move with
      | Pre_Promotion (x,y) -> Full_Promotion (pc_type,x,y)
      | _ -> move
    in
    let new_move = get_full_promotion move pc_type in
    (execute state new_move piece), new_move

let get_corresponding_mv_pc pos1 pos2 mv_lst state =
  let rec get_move mv_lst pos state =
    match mv_lst with
    |[] -> None
    |move::t -> if (helper_move move state = pos) then Some move else get_move t pos state
  in
  let move = get_move mv_lst pos2 state in
  let piece = get_piece state pos1 in
  move, piece

let get_turn st =
  match st.turn with
  | White -> "White"
  | Black -> "Black"

let opposite_turn st =
  match st.turn with
  | White -> "Black"
  | Black -> "White"

let is_empty pos st = get_piece st pos = None


(*--------------------------------- For AI ------------------------------------*)

let copy_state st =
  make_state st.turn st.white_king_pos st.black_king_pos (Hashtbl.copy st.board) st.enPassant_target
(*
let get_pieces st c =

*)


 (* ------ TEST ------ *)

let remove pos st =
  let b = (Hashtbl.add st.board pos None); st.board in
  make_state st.turn st.white_king_pos st.black_king_pos b st.enPassant_target

let addP pos p st =
  let b = (Hashtbl.add st.board pos None); (Hashtbl.add st.board pos p); st.board in
  make_state st.turn st.white_king_pos st.black_king_pos b st.enPassant_target

let get_piece_type (p:piece) =
  p.piece_type
