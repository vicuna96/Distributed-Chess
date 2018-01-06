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
(* [apply_move mv p b] returns a new [board] that has [mv] applied [p].
    raises [Invalid_Move] exception if invalid {move} type.
    IMPORTANT: This method mutates the original [board] [b]. *)
val apply_move : move -> piece -> board ->  board

(* [get_possible_moves pos st] is a list of move in [move list] that is
    a list of all possible [move] that can be "physically" possible from
    that [position]. "Physically possible" means any possible move that DOES NOT
    account the possibility that the move will result the [King] to be immedately
    checked. It however checks all possible moves with all different special moves. *)
val get_possible_moves : position -> state -> move list

(* [is_checkmate st] is a [bool] that tells whether in the current [state]
   the player with corresponding [turn] is in checkmate. *)
val is_checkmate : state -> bool


(* [execute st mv p] is a new [state] that the [move] has been executed on
   [p]. Requires: [move] is a valid move (i.e., [move] that is possible and does
   not result check.) *)
val execute : state -> move -> piece -> state


(*   ------------------------------- GUI -----------------------------------   *)

(* [encode (mv,str)] is a string to send to the other player over server that
   contains the last executed move. This will be used to update the other player's
   state. Inspired by FEN format. *)
val encode : move * string -> string

(* [decode str st] is a nested tuple type that will be useful to update the state
   of current player. When the turn has just been passed to the current user, the
   user will decode the string sent over the server and decode. The [move] is
   what the opponent has done, [string option] is possible promotion target, and
   [piece option] is the [piece] that the opponent moved. It is ideally guaranteed
   to always have [Some piece]. *)
val decode : string -> state -> piece option * (move * string option)

(* [get_valid_moves st pos] is a list of valid moves in [move list]. This [move list]
   is different from [get_possible_moves pos st] because in this case the possibility
   that the king might be checked is taken care of. *)
val get_valid_moves : state -> position -> move list

(* [get_valid_squares mv_lst st] is the list of [position] in [position list] that
   is used to shade the possible squares on the chessboard to move in gui. *)
val get_valid_squares : move list -> state -> position list

(* [opposite_turn st] is a string representation of the OPPOSITE of the current
   [turn]. That is, if the current turn is [White], then it is ["Black"], and vice
   versa. *)
val opposite_turn : state -> string

(* [get_turn st] is a string representation of the current [turn]. i.e., if
   the current [turn] is [White] then it is ["White"]. *)
val get_turn : state -> string

(* [get_corresponding_mv_pc pos1 pos2 mv_lst st] is a tuple of [move option] and
   [piece option] that tells the corresponding [move] and [piece] from two positons
   "position from" and "positon to", and the list of [move] from [st]. This function
   is used when a VALID list of [move list] is given with corresponding [state],
   and after user activated the second click so the positions from and to are known,
   this function checks if the two positions [pos1] and [pos2] will be a valid
   [move] by checking from [mv_lst]. If it there is a piece in [pos1], it will be
   the [snd] of the tuple, and if the move is valid (i.e., there is a valid move in
   [mv_lst]) then that will be the [fst] of the tuple. *)
val get_corresponding_mv_pc : position -> position -> move list -> state -> move option * piece option

(* [get_piece_map st] will return GUI friendly format of association list that
   each element is a tuple of [int * int] and [string * string], which is
   integer tuple representation of [position] and string tuple representation of
   [color * piece_type].  *)
val get_piece_map : state -> ((int * int) * (string * string)) list

(* [king_cur_check st] is [bool] if the king of the current player is currently
   checked. *)
val king_cur_check : state -> bool

(* [is_PromotionMove mv] is [bool] that tells if [mv] is a promotion mve, which
   is [Pre_Promotion of position * position]. *)
val is_PromotionMove : move -> bool

(* [move st mv p str] is a tuple of [state] and [move] that returns a new
   [state] that [mv] has been applied on [p]. [move] is the final form of [mv].
   This exists to communicate with GUI only for the promotion event. When the
   user wants to promote a [Pawn], then [mv] will be [Pre_Promotion] and user
   will be prompted to click the choice of promotion. That choice will be passed
   as the argument [string option] and will be used to form [move] that is
   [Full_Promotion of piece_type * position * position]. *)
val move : state -> move -> piece -> string option -> state * move

(* [is_empty pos st] is [bool] that tells if the [pos] of [st] is empty. (i.e.,
   there is no [piece] in the [board] of [st]. *)
val is_empty : position -> state -> bool

(* [piece_from_opt po] is an unwrapping function that unwraps option.
   raises exception if [piece option] is [None]. *)
val piece_from_op : piece option -> piece

(* [init_state ()] is an initial [state], with all setting new. *)
val init_state : unit -> state

(* ----- AI ------ *)
(* [copy_state st] COPIES [st]. *)
val copy_state : state -> state




(* --- TEST ----- *)

val get_next : piece_color -> piece_color

val get_dir : piece_type -> piece_color -> direction

val init_board : unit -> board

val get_piece : state -> position -> piece option

val get_board : state -> board

(*[get_color piece] is the color of pieces [piece] *)
val get_color : piece -> piece_color

val get_turn_color : state -> piece_color

(*[get_piece_type piece] is the type of [piece] *)
val get_piece_type : piece -> piece_type


val remove : position -> state -> state

val addP : position -> piece option -> state -> state

val get_piece_type : piece -> piece_type

val get_value : piece_type -> int
