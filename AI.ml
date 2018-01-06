type time = Early | Late

let pawn_scores x y =
  let scores =
[|
  [| 0;  0;  0;  0;  0;  0;  0;  0|];
  [| 50; 50; 50; 50; 50; 50; 50; 50|];
  [| 10; 10; 20; 30; 30; 20; 10; 10|];
  [| 5;  5; 10; 27; 27; 10;  5;  5|];
  [| 0;  0;  0; 25; 25;  0;  0;  0|];
  [| 5; -5;-10;  0;  0;-10; -5;  5|];
  [| 5; 10; 10;-25;-25; 10; 10;  5|];
  [| 0;  0;  0;  0;  0;  0;  0;  0|]
|]
  in scores.(8-y).(x-1)

let knight_scores x y =
  let scores =
  [|
    [|-50;-40;-30;-30;-30;-30;-40;-50;|];
    [|-40;-20;  0;  0;  0;  0;-20;-40;|];
    [|-30;  0; 10; 15; 15; 10;  0;-30;|];
    [|-30;  5; 15; 20; 20; 15;  5;-30;|];
    [|-30;  0; 15; 20; 20; 15;  0;-30;|];
    [|-40;-20;  0;  5;  5;  0;-20;-40;|];
    [|-30;  5; 10; 15; 15; 10;  5;-30;|];
    [|-50;-40;-20;-30;-30;-20;-40;-50;|];
  |]
  in scores.(8-y).(x-1)


let bishop_scores x y =
  let scores =
  [|
    [|-20;-10;-10;-10;-10;-10;-10;-20;|];
    [|-10;  0;  0;  0;  0;  0;  0;-10;|];
    [|-10;  0;  5; 10; 10;  5;  0;-10;|];
    [|-10;  5;  5; 10; 10;  5;  5;-10;|];
    [|-10;  0; 10; 10; 10; 10;  0;-10;|];
    [|-10; 10; 10; 10; 10; 10; 10;-10;|];
    [|-20;-10;-40;-10;-10;-40;-10;-20;|];
    [|-10;  5;  0;  0;  0;  0;  5;-10;|];
  |]
  in scores.(8-y).(x-1)

let kingscores_early x y =
  let scores =
  [|
    [|-30; -40; -40; -50; -50; -40; -40; -30;|];
    [|-30; -40; -40; -50; -50; -40; -40; -30;|];
    [|-30; -40; -40; -50; -50; -40; -40; -30;|];
    [|-30; -40; -40; -50; -50; -40; -40; -30;|];
    [|-20; -30; -30; -40; -40; -30; -30; -20;|];
    [|-10; -20; -20; -20; -20; -20; -20; -10;|];
    [| 20;  20;   0;   0;   0;   0;  20;  20;|];
    [| 20;  30;  10;   0;   0;  10;  30;  20|];
  |]
  in scores.(8-y).(x-1)

let kingscores_late x y =
  let scores =
  [|
    [|-50;-40;-30;-20;-20;-30;-40;-50;|];
    [|-30;-10; 20; 30; 30; 20;-10;-30;|];
    [|-30;-10; 30; 40; 40; 30;-10;-30;|];
    [|-30;-20;-10;  0;  0;-10;-20;-30;|];
    [|-30;-10; 30; 40; 40; 30;-10;-30;|];
    [|-30;-10; 20; 30; 30; 20;-10;-30;|];
    [|-30;-30;  0;  0;  0;  0;-30;-30;|];
    [|-50;-30;-30;-30;-30;-30;-30;-50|];
  |]
  in scores.(8-y).(x-1)

let rec helper board x y (accu(*):(Board.position*Board.piece) list)*)) color =
  match x with
  | 9 -> accu
  | _ ->
    match y with
    | 9 -> helper board (x+1) 1 accu color
    | _ -> add_piece (x,y) accu color board

and add_piece (x,y) accu color board =
  let piece = Hashtbl.find board (x,y) in
  match piece with
  | Some z when z|>Board.get_color = color ->
    let accu' = ((x,y),z)::accu in
      helper board x (y+1) accu' color
  | Some _ | None -> helper board x (y+1) accu color

let get_pieces st color (*): (Board.position*Board.piece) list *)=
  let board = st|>Board.get_board in
  helper board 1 1 [] color

let rec flatten' accu (x,lst) (*): (Board.piece*Board.move) list *)=
  match lst with
  | [] -> accu
  | h::t -> flatten' ((x,h)::accu) (x,t)


let get_frontier st color (*):  )(Board.piece*Board.move) list *)=
  let pieces = get_pieces st color in
  let add_moves ((x,y),z) = z,((x,y)|>Board.get_valid_moves st) in
  pieces
  |> List.map add_moves
  |> List.map (flatten' [])
  |> List.flatten


let rec max_state accu opt (lst:((Board.piece*Board.move)*int) list) =
  match lst with
  | [] -> opt
  | ((x,y),z)::t when z>=accu-> max_state z (Some (x,y)) t
  | _::t -> max_state accu opt t

let get_pscore (x,y) tim color piece_t: int = let open Board in
    let (x',y')=
    if color = Board.Black then
      9-x,9-y
    else
      x,y
    in
    match piece_t with
    | Queen | Rook -> 0
    | Bishop -> bishop_scores x' y'
    | Knight -> knight_scores x' y'
    | Pawn -> pawn_scores x' y'
    | King when tim=Early -> kingscores_early x' y'
    | King when tim=Late -> kingscores_late x' y'
    |  _ -> failwith "That's all the piece types"

let utility (st:Board.state) color : int =
  let pieces = get_pieces st color in
  let score a ((x,y),z) =
    a + (get_pscore (x,y) Early color (z|>Board.get_piece_type)) + (z|>Board.get_piece_type|>Board.get_value) in
  List.fold_left (score) 0 pieces

let unwrap board color mov = let open Board in
  match mov with
  | Pre_Promotion (x,y)-> Full_Promotion (Queen,x,y), Some "Queen"
  | EnPassant (x,y) -> EnPassant (x,y),None
  | Normal (x,y) ->
    begin
      (*)
      let piece_opt = Hashtbl.find board y in

      let score =
        match piece_opt with
        | None -> 0
        | Some z ->
          if z|>get_color = get_next color then
            z|>get_piece_type|>get_value
          else 0
        in*) Normal (x,y), None
    end
  | _ -> mov,None


let rec max_value st a b depth max_dep color accu (v:int) =
  match accu with
  | [] -> v
  | (piece,act)::t ->
    let mv',opt = unwrap (st|>Board.get_board) color act in
    let st' = Board.copy_state st in
    let st'',mv'' = (Board.move st' mv' piece opt) in (*    Carefull here!!!! *)
    let color' = Board.get_next color in
    if depth=max_dep then
      (utility st'' color)-(utility st'' color')
    else
      let actions = get_frontier st' color' in
      let value' =
        min_value st' a b (depth+1) max_dep color' actions (max_int) in
      let max_val = max v value' in
      if max_val >= b then
        max_val
      else
        max_value st (max a max_val) b depth max_dep color t max_val

and min_value st a b depth max_dep color accu v =
  match accu with
  | [] -> v
  | (piece,act)::t ->
    let mv',opt = unwrap (st|>Board.get_board) color act in
    let st' = Board.copy_state st in
    let st'',mv'' = (Board.move st' mv' piece opt) in (*    Carefull here!!!! *)
    let color' = Board.get_next color in
    if depth=max_dep then
      (utility st'' color')-(utility st'' color)
    else
      let color' = Board.get_next color in
      let actions = get_frontier st'' color' in
      let value' =
        max_value st' a b (depth+1) max_dep color' actions (min_int) in
      let min_val = min v value' in
      if min_val <= a then
        min_val
      else
        min_value st a (min b min_val) depth max_dep color t min_val


let rec alpha_beta_decision st depth color =
  color
  |> get_frontier st
  |> List.map (apply st color depth)
  |> max_state min_int None


and apply st color depth (piec,mov) : ((Board.piece*Board.move)*int) =
  let mv',opt = unwrap (Board.get_board st) color mov in
  let st' = Board.copy_state st in
  let st'',mv = (Board.move st' mv' piec opt) in (*    Carefull here!!!! *)
  let color' = Board.get_next color in
  let frontier = get_frontier st'' color' in
  (piec,mov),(min_value st'' min_int max_int 0 depth color' frontier max_int)

let rec alpha_beta_decision' st depth color =
  let frontier = get_frontier st color in
  apply' st color min_int max_int depth frontier None min_int min_int

and apply' st color a b depth accu best mi ma  =
  match accu with
  | [] -> best
  | (piece,act)::t ->
  let mv',opt = unwrap (Board.get_board st) color act in
  let st' = Board.copy_state st in
  let st'',mv = (Board.move st' mv' piece opt) in (*    Carefull here!!!! *)
  let color' = Board.get_next color in
  let frontier = get_frontier st'' color' in
  let value' = min_value st'' a b 0 depth color' frontier min_int in
  let min_val = min mi value' in
  if value'<ma then
    apply' st color a (min b min_val) depth t best min_val ma
  else
    apply' st color a (min b min_val) depth t (Some (piece,act)) min_val value'


let compute_next st color depth =
  let bmove = alpha_beta_decision st depth color in
  match bmove with
  | None -> raise Not_found
  | Some (piece,mv) ->
    let mv',opt = unwrap (st|>Board.get_board) color mv in
    Board.move st mv' piece opt
