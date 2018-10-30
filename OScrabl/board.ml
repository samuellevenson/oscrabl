open ANSITerminal
open Words

exception Can'tPlaceTile
exception InvalidSquare
exception InvalidRow
exception InvalidColumn
exception NothingSquare
exception InvalidWord of string
exception InvalidTilePlacement

(** The type of tiles *)
type pretile = {
  letter: string; value: int
}
type tile = Final of pretile | Unfinal of pretile | Nothing

(** [tile_style] is the ANSITerminal style list for letter tiles *)
let tile_style = [Bold; white; on_black]

(** The type of score multipliers *)
type multiplier = DoubleLetter | TripleLetter | DoubleWord | TripleWord | NaN

(** The type of squares *)
type square = tile * multiplier

(** The type of the scrabble® board *)
type board = square list list

(** The square list that represents the 0th column of a scrabble board at the
    start of a game *)
let def_col_zero : square list =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row = 0) then helper (row + 1) ((Nothing, TripleWord)::acclist)
        else if (row <> 0) && (row < 3) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 3) then helper (row + 1) ((Nothing, DoubleLetter)::acclist)
        else if (row <> 3) && (row < 7) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 7) then helper (row + 1) ((Nothing, TripleWord)::acclist)
        else if (row <> 7) && (row < 11) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 11) then helper (row + 1) ((Nothing, DoubleLetter)::acclist)
        else if (row <> 11) && (row < 14) then helper (row + 1) ((Nothing, NaN)::acclist)
        else helper (row + 1) ((Nothing, TripleWord)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 1st column of a scrabble board at the
    start of a game *)
let def_col_one =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row = 0) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 1) then helper (row + 1) ((Nothing, DoubleWord)::acclist)
        else if (row < 5) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 5) then helper (row + 1) ((Nothing, TripleLetter)::acclist)
        else if (row < 9) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 9) then helper (row + 1) ((Nothing, TripleLetter)::acclist)
        else if (row < 13) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 13) then helper (row + 1) ((Nothing, DoubleWord)::acclist)
        else helper (row + 1) ((Nothing, NaN)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 2nd column of a scrabble board at the
    start of a game *)
let def_col_two =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row < 2) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 2) then helper (row + 1) ((Nothing, DoubleWord)::acclist)
        else if (row < 6) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 6) then helper (row + 1) ((Nothing, DoubleLetter)::acclist)
        else if (row < 8) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 8) then helper (row + 1) ((Nothing, DoubleLetter)::acclist)
        else if (row < 12) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 12) then helper (row + 1) ((Nothing, DoubleWord)::acclist)
        else helper (row + 1) ((Nothing, NaN)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 3rd column of a scrabble board at the
    start of a game *)
let def_col_three =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row = 0) then helper (row + 1) ((Nothing, DoubleLetter)::acclist)
        else if (row < 3) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 3) then helper (row + 1) ((Nothing, DoubleWord)::acclist)
        else if (row < 7) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 7) then helper (row + 1) ((Nothing, DoubleLetter)::acclist)
        else if (row < 11) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 11) then helper (row + 1) ((Nothing, DoubleWord)::acclist)
        else if (row < 14) then helper (row + 1) ((Nothing, NaN)::acclist)
        else helper (row + 1) ((Nothing, DoubleLetter)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 4th column of a scrabble board at the
    start of a game *)
let def_col_four =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row < 4) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 4) then helper (row + 1) ((Nothing, DoubleWord)::acclist)
        else if (row < 10) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 10) then helper (row + 1) ((Nothing, DoubleWord)::acclist)
        else helper (row + 1) ((Nothing, NaN)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 5th column of a scrabble board at the
    start of a game *)
let def_col_five =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row = 0) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 1) then helper (row + 1) ((Nothing, TripleLetter)::acclist)
        else if (row < 5) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 5) then helper (row + 1) ((Nothing, TripleLetter)::acclist)
        else if (row < 9) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 9) then helper (row + 1) ((Nothing, TripleLetter)::acclist)
        else if (row < 13) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 13) then helper (row + 1) ((Nothing, TripleLetter)::acclist)
        else helper (row + 1) ((Nothing, NaN)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 6th column of a scrabble board at the
    start of a game *)
let def_col_six =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row < 2) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 2) then helper (row + 1) ((Nothing, DoubleLetter)::acclist)
        else if (row < 6) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 6) then helper (row + 1) ((Nothing, DoubleLetter)::acclist)
        else if (row < 8) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 8) then helper (row + 1) ((Nothing, DoubleLetter)::acclist)
        else if (row < 12) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 12) then helper (row + 1) ((Nothing, DoubleLetter)::acclist)
        else helper (row + 1) ((Nothing, NaN)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 7th column of a scrabble board at the
    start of a game *)
let def_col_seven =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row = 0) then helper (row + 1) ((Nothing, TripleWord)::acclist)
        else if (row < 3) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 3) then helper (row + 1) ((Nothing, DoubleLetter)::acclist)
        else if (row < 7) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 7) then helper (row + 1) ((Nothing, DoubleWord)::acclist)
        else if (row < 11) then helper (row + 1) ((Nothing, NaN)::acclist)
        else if (row = 11) then helper (row + 1) ((Nothing, DoubleLetter)::acclist)
        else if (row < 14) then helper (row + 1) ((Nothing, NaN)::acclist)
        else helper (row + 1) ((Nothing, TripleWord)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list list that represents the scrabble board at the start of a
    game *)
let emptyBoard =
  [def_col_zero;def_col_one;def_col_two;def_col_three;def_col_four;def_col_five;
   def_col_six;def_col_seven;def_col_six;def_col_five;def_col_four;
   def_col_three;def_col_two;def_col_one;def_col_zero]

(** [insertTile b t (x,y)] is the scrabble board [b] with a tile [t] square
    at the [x]th row and [y]th column of the board *)
let insertTile board tile (x,y) =
  if fst (List.nth (List.nth board x) y) <> Nothing then raise Can'tPlaceTile else
    let rec rowIter row col new_col =
      if row < y then rowIter (row + 1) col ((List.nth col row)::new_col)
      else if row = y then
        let multi = match (List.nth col row) with
          | (a, b) -> b in rowIter (row + 1) col ((tile, multi)::new_col)
      else if row < 15 then rowIter (row + 1) col ((List.nth col row)::new_col)
      else List.rev new_col in
    let rec columnIter col list_of_cols =
      if col < x then columnIter (col + 1) ((List.nth board col)::list_of_cols)
      else if col = x then columnIter (col + 1) ((rowIter 0 (List.nth board col) [])::list_of_cols)
      else if col < 15 then columnIter (col + 1) ((List.nth board col)::list_of_cols)
      else List.rev list_of_cols in
    columnIter 0 []

(** [print_topline line] prints the top half of [line], where [line] is one row
    of a board *)
let rec print_topline line =
  match line with
  | [] -> print_endline "|"
  | x::xs -> match x with
    | Nothing, NaN -> print_string [] "|    "; print_topline xs
    | Nothing, DoubleLetter ->
      print_string [] ("|");
      print_string [on_cyan] (" 2  "); print_topline xs
    | Nothing, TripleLetter ->
      print_string [] ("|");
      print_string [on_blue] (" 3  "); print_topline xs
    | Nothing, DoubleWord ->
      print_string [] ("|");
      print_string [on_magenta] (" 2  "); print_topline xs
    | Nothing, TripleWord ->
      print_string [] ("|");
      print_string [on_red] (" 3  "); print_topline xs
    | Final tile, _ | Unfinal tile, _ ->
      print_string [] ("|");
      print_string tile_style (" " ^ tile.letter ^ "  ");
      print_topline xs

(** [offset tile] is the spaces needed after the value of a tile in order to
    account for differences in number of digits.*)
let offset tile =
  if tile.value >= 10 then "" else " "

(** [print_topline line] prints the bottom half of [line], where [line] is one
    row of a board *)
let rec print_botline line =
  match line with
  | [] -> print_endline "|"
  | x::xs -> match x with
    | Nothing, NaN -> print_string [] "|    "; print_botline xs
    | Nothing, DoubleLetter ->
      print_string [] ("|");
      print_string [on_cyan] ("  L "); print_botline xs
    | Nothing, TripleLetter ->
      print_string [] ("|");
      print_string [on_blue] ("  L "); print_botline xs
    | Nothing, DoubleWord ->
      print_string [] ("|");
      print_string [on_magenta] ("  W "); print_botline xs
    | Nothing, TripleWord ->
      print_string [] ("|");
      print_string [on_red] ("  W "); print_botline xs
    | Final tile, _ | Unfinal tile, _ ->
      print_string [] ("|");
      print_string tile_style ("  " ^ string_of_int tile.value ^ offset tile);
      print_botline xs

(** [print_linenum i] prints the character corresponding to the row number with
    A for row 1, B for row 2, and so on *)
let print_linenum i =
  print_string [] ((i + 65) |> Char.chr |> Char.escaped)

(** [print_board board] prints a graphical representation of [board] into the
    terminal window *)
let rec print_board board i =
  let rec print_iter board i =
    print_endline " +————+————+————+————+————+————+————+————+————+————+————+————+————+————+————+";
    match board with
    | [] -> ()
    | x::xs ->
      print_linenum i; print_topline x; print_string [] " ";
      print_botline x; print_iter xs (i + 1)
  in print_endline "   0    1    2    3    4    5    6    7    8    9    10   11   12   13   14";
  print_iter board i

(** [tileSqrs_to_preTileSqrs sqrList] returns [sqrList] with all [Final(a)] and
    [Unfinal(a)] tiles converted to [a] pretiles. *)
let tileSqrs_to_preTileSqrs squareList =
  let rec helper (sqrList: square list) (accList: (pretile * multiplier) list) =
    match squareList with
    | (Final(pt), multiplier)::t -> let ptSqr = (pt, multiplier) in
      helper t (ptSqr::accList)
    | (Unfinal(pt), multiplier)::t -> let ptSqr = (pt, multiplier) in
      helper t (ptSqr::accList)
    | (Nothing, multiplier)::t -> helper t accList
    | _ -> List.rev accList
  in helper squareList []

(**[squares_to_wordpoints sqL] returns the string that is formed by the list of
   tiles from [sqL], and the point value for the string after factoring in
   multiplers **)
let squares_to_word_and_points squareList =
  let pretileSqrList = tileSqrs_to_preTileSqrs squareList in
  let rec helper tL accStr accPts accMults=
    match tL with
    | ({letter; value},multiplier)::t -> let int_word = accStr ^ letter in
      begin match multiplier with
        | DoubleLetter -> helper t int_word (accPts + (value * 2)) accMults
        | TripleLetter -> helper t int_word (accPts + (value * 3)) accMults
        | DoubleWord -> helper t int_word (accPts + (value)) (accMults * 2)
        | TripleWord -> helper t int_word (accPts + (value)) (accMults * 3)
        | NaN -> helper t int_word (accPts + (value)) (accMults)
      end
    | _ -> (accStr, (accPts * accMults)) in
  helper pretileSqrList "" 0 1

(**[is_word s] returns [true] if [s] is a string in dictionary.json,
   otherwise [false]. *)
let is_word str =
  (Words.validity str word_set)

(**[all_are_words strList] returns [true] if all strings in [strList] are in the
   dictionary.json, otherwise [false] *)
let rec all_are_words strList =
  match strList with
  | h::t -> if (is_word h) then (all_are_words t) else false
  | _ -> true

(**[get_square brd (x,y)] returns the square at coordinate position [(x,y)] on
   [brd]. Raises [Invalid_Square] if the square does not exist. *)
let get_square board (x,y) = if (x >= 0 && x<= 14) && (y>=0 && y<= 14) then
    List.nth (List.nth board x) y else raise InvalidSquare

(**[get_row_sqrs board y] returns the list of squares of row [y] on [board].
   Raises [Invalid_Row] if the row does not exist. *)
let get_row_sqrs (board:board) y =
  if (y>=0 && y<15) then
    let rec helper xPos accList =
      if (xPos < 15)
      then helper (xPos + 1) (accList@[List.nth (List.nth board xPos) y])
      else accList
    in helper 0 []
  else raise InvalidRow

(**[get_col_sqrs board x] returns the list of squares of column [x] on [board].
   Raises [Invalid_Column] if the column does not exist. *)
let get_col_sqrs (board:board) x =
  if (x>= 0 && x<15) then List.nth board x
  else raise InvalidColumn

(**[get_rowadj_notNothing_sqrs board (x,y)] returns the list of horizontally
   connected squares that don't contain any Nothing tiles. The list includes the
   square at position [(x,y)] on [board]. If the square at position [(x,y)]
   contains a Nothing tile, raises [NothingSquare]. *)
let get_rowadj_notNothing_sqrs board (x,y) =
  let sqr = get_square board (x,y) in
  match sqr with
  | (Nothing, multiplier) -> raise NothingSquare
  | _ -> begin
      let rec rightHelper xPos (accSqrList: square list) =
        if (xPos<15) then (let tentSqr = get_square board (xPos,y) in
                           match tentSqr with
                           | (Nothing, multiplier) -> accSqrList
                           | _ -> rightHelper (xPos+1) (accSqrList@[tentSqr]))
        else accSqrList in
      let rec leftHelper xPos (accSqrList: square list) =
        if (xPos>=0) then (let tentSqr = get_square board (xPos,y) in
                           match tentSqr with
                           | (Nothing, multiplier) -> accSqrList
                           | _ -> leftHelper (xPos-1) (tentSqr::accSqrList))
        else accSqrList in
      let rightList = rightHelper (x+1) [sqr] in
      let leftList = leftHelper (x-1) [] in
      leftList@rightList
    end

(**[get_coladj_notNothing_sqrs board (x,y)] returns the list of vertically
   connected squares that don't contain any Nothing tiles. The list includes the
   square at position [(x,y)] on [board]. If the square at position [(x,y)]
   contains a Nothing tile, raises [NothingSquare]. *)
let get_coladj_notNothing_sqrs board (x,y) =
  let sqr = get_square board (x,y) in
  match sqr with
  | (Nothing, multiplier) -> raise NothingSquare
  | _ -> begin
      let rec botHelper yPos (accSqrList: square list) =
        if (yPos<15) then (let tentSqr = get_square board (x,yPos) in
                           match tentSqr with
                           | (Nothing, multiplier) -> accSqrList
                           | _ -> botHelper (yPos+1) (accSqrList@[tentSqr]))
        else accSqrList in
      let rec topHelper yPos (accSqrList: square list) =
        if (yPos>=0) then (let tentSqr = get_square board (x,yPos) in
                           match tentSqr with
                           | (Nothing, multiplier) -> accSqrList
                           | _ -> topHelper (yPos-1) (tentSqr::accSqrList))
        else accSqrList in
      let botList = botHelper (y+1) [sqr] in
      let topList = topHelper (y-1) [] in
      topList@botList
    end

(** [row_is_connected board y] is true when the row does not contain any squares
    of type Nothing in between the Unfinal squares *)
let row_is_connected (board:board) y =
  let rec row_iter (passed_unfinal:bool) (passed_nothing:bool) (row:square list) =
    match row with
    | [] -> true
    | x::xs -> match fst x with
      | Unfinal _ when passed_nothing -> false
      | Unfinal _ -> row_iter true passed_nothing xs
      | Final _ -> row_iter passed_unfinal passed_nothing xs
      | Nothing when passed_unfinal -> row_iter passed_unfinal true xs
      | Nothing -> row_iter passed_unfinal passed_nothing xs
  in row_iter false false (get_row_sqrs board y)

(** [col_is_connected board y] is true when the col does not contain any squares
    of type Nothing in between the Unfinal squares *)
let col_is_connected (board:board) x =
  let rec col_iter (passed_unfinal:bool) (passed_nothing:bool) row =
    match row with
    | [] -> true
    | x::xs -> match fst x with
      | Unfinal _ when passed_nothing -> false
      | Unfinal _ -> col_iter true passed_nothing xs
      | Final _ -> col_iter passed_unfinal passed_nothing xs
      | Nothing when passed_unfinal -> col_iter passed_unfinal true xs
      | Nothing -> col_iter passed_unfinal passed_nothing xs
  in col_iter false false (get_col_sqrs board x)


(** finds the position of some unfinal tile on the board, returns it as (x,y) *)
let rec find_unfinal board: (int * int) =
  let rec board_iter x y =
    match fst (get_square board (x, y)) with
    | Unfinal tile -> (x,y)
    | _ ->
      if x < 14 then (board_iter (x+1) y)
      else if y < 14 then (board_iter 0 (y+1))
      else raise InvalidTilePlacement
  in board_iter 0 0

(** returns true if all squares outside of the cross centered on (x,y) do not
    contain unfinal tiles *)
let check_uncrossed board (x_fix, y_fix) =
  let rec board_iter x y =
    match fst (get_square board (x, y)) with
    | Unfinal tile ->
      if x <> x_fix && y <> y_fix then false
      else if x < 14 then board_iter (x+1) y
      else if y < 14 then board_iter 0 (y+1)
      else true
    | _ ->
      if x < 14 then board_iter (x+1) y
      else if y < 14 then board_iter 0 (y+1)
      else true
  in board_iter 0 0

(** prefix exclusive or operator *)
let xor p1 p2 =
  (p1 && not p2) || (not p1 && p2)

(** [valid_tile_positions board] is whether the tiles of [board] are placed in
    a valid configuration by the rules of Scrabble®
*)
let valid_tile_positions board: bool =
  let (x,y) = find_unfinal board in
  check_uncrossed board (x,y) &&
  (xor (List.length (get_rowadj_notNothing_sqrs board (x,y)) > 1)
     (List.length (get_coladj_notNothing_sqrs board (x,y)) > 1))
  && row_is_connected board y && col_is_connected board x

(** [find_words board] finds all the "words" created by the unfinal tiles on the
    board. These words may not be English words *)
let find_words board : square list list =
  let rec board_iter x y words_acc =
    match fst (get_square board (x, y)) with
    | Unfinal tile ->
      let to_add =
        [(get_coladj_notNothing_sqrs board (x,y));(get_rowadj_notNothing_sqrs board (x,y))] in
      if x < 14 then (board_iter (x+1) y) (to_add@words_acc)
      else if y < 14 then (board_iter 0 (y+1)) (to_add@words_acc)
      else (to_add@words_acc)
    | _ ->
      if x < 14 then (board_iter (x+1) y) words_acc
      else if y < 14 then (board_iter 0 (y+1)) words_acc
      else words_acc
  in (board_iter 0 0 []) |> List.filter (fun x -> List.length x > 1) |> List.sort_uniq compare

(** Score? *)
let calc_score board : int =
  let rec words_iter assoc score_acc =
    match assoc with
    | [] -> score_acc
    | (word,score)::xs ->
      if Words.validity word word_set then words_iter xs (score_acc + score)
      else raise (InvalidWord word)
  in
  if valid_tile_positions board
  then words_iter (List.map squares_to_word_and_points (find_words board)) 0
  else raise InvalidTilePlacement

(** [finalize_board board] turns all the Unfinal tiles into Final tiles *)
let finalize board =
  board
