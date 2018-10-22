open ANSITerminal

(** The type of tiles *)
type tile = {
  letter: string; value: int
}

(** The type of score multipliers *)
type multiplier = DoubleLetter | TripleLetter | DoubleWord | TripleWord | NaN

(** The type of squares *)
type square = (tile option) * (multiplier)

(** The type of the scrabble® board *)
type board = square list list

(** The square list that represents the 0th column of a scrabble board at the 
    start of a game *)
let def_col_zero : square list =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row = 0) then helper (row + 1) ((None, TripleWord)::acclist)
        else if (row <> 0) && (row < 3) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 3) then helper (row + 1) ((None, DoubleLetter)::acclist)
        else if (row <> 3) && (row < 7) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 7) then helper (row + 1) ((None, TripleWord)::acclist)
        else if (row <> 7) && (row < 11) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 11) then helper (row + 1) ((None, DoubleLetter)::acclist)
        else if (row <> 11) && (row < 14) then helper (row + 1) ((None, NaN)::acclist)
        else helper (row + 1) ((None, TripleWord)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 1st column of a scrabble board at the 
    start of a game *)
let def_col_one =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row = 0) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 1) then helper (row + 1) ((None, DoubleWord)::acclist)
        else if (row < 5) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 5) then helper (row + 1) ((None, TripleLetter)::acclist)
        else if (row < 9) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 9) then helper (row + 1) ((None, TripleLetter)::acclist)
        else if (row < 13) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 13) then helper (row + 1) ((None, DoubleWord)::acclist)
        else helper (row + 1) ((None, NaN)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 2nd column of a scrabble board at the 
    start of a game *)
let def_col_two =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row < 2) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 2) then helper (row + 1) ((None, DoubleWord)::acclist)
        else if (row < 6) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 6) then helper (row + 1) ((None, DoubleLetter)::acclist)
        else if (row < 8) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 8) then helper (row + 1) ((None, DoubleLetter)::acclist)
        else if (row < 12) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 12) then helper (row + 1) ((None, DoubleWord)::acclist)
        else helper (row + 1) ((None, NaN)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 3rd column of a scrabble board at the 
    start of a game *)
let def_col_three =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row = 0) then helper (row + 1) ((None, DoubleLetter)::acclist)
        else if (row < 3) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 3) then helper (row + 1) ((None, DoubleWord)::acclist)
        else if (row < 7) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 7) then helper (row + 1) ((None, DoubleLetter)::acclist)
        else if (row < 11) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 11) then helper (row + 1) ((None, DoubleWord)::acclist)
        else if (row < 14) then helper (row + 1) ((None, NaN)::acclist)
        else helper (row + 1) ((None, DoubleLetter)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 4th column of a scrabble board at the 
    start of a game *)
let def_col_four =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row < 4) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 4) then helper (row + 1) ((None, DoubleWord)::acclist)
        else if (row < 10) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 10) then helper (row + 1) ((None, DoubleWord)::acclist)
        else helper (row + 1) ((None, NaN)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 5th column of a scrabble board at the 
    start of a game *)
let def_col_five =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row = 0) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 1) then helper (row + 1) ((None, TripleLetter)::acclist)
        else if (row < 5) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 5) then helper (row + 1) ((None, TripleLetter)::acclist)
        else if (row < 9) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 9) then helper (row + 1) ((None, TripleLetter)::acclist)
        else if (row < 13) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 13) then helper (row + 1) ((None, TripleLetter)::acclist)
        else helper (row + 1) ((None, NaN)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 6th column of a scrabble board at the 
    start of a game *)
let def_col_six =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row < 2) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 2) then helper (row + 1) ((None, DoubleLetter)::acclist)
        else if (row < 6) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 6) then helper (row + 1) ((None, DoubleLetter)::acclist)
        else if (row < 8) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 8) then helper (row + 1) ((None, DoubleLetter)::acclist)
        else if (row < 12) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 12) then helper (row + 1) ((None, DoubleLetter)::acclist)
        else helper (row + 1) ((None, NaN)::acclist)
      end
    else List.rev acclist
  in helper 0 []

(** The square list that represents the 7th column of a scrabble board at the 
    start of a game *)
let def_col_seven =
  let rec helper row acclist =
    if (row < 15) then
      begin
        if (row = 0) then helper (row + 1) ((None, TripleWord)::acclist)
        else if (row < 3) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 3) then helper (row + 1) ((None, DoubleLetter)::acclist)
        else if (row < 7) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 7) then helper (row + 1) ((None, DoubleWord)::acclist)
        else if (row < 11) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 11) then helper (row + 1) ((None, DoubleLetter)::acclist)
        else if (row < 14) then helper (row + 1) ((None, NaN)::acclist)
        else helper (row + 1) ((None, TripleWord)::acclist)
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
    | None, NaN -> print_string [] "|    "; print_topline xs
    | None, DoubleLetter -> print_string [] ("|"); print_string [white; on_cyan] (" 2  "); print_topline xs
    | None, TripleLetter -> print_string [] ("|"); print_string [white; on_blue] (" 3  "); print_topline xs
    | None, DoubleWord -> print_string [] ("|"); print_string [white; on_magenta] (" 2  "); print_topline xs
    | None, TripleWord -> print_string [] ("|"); print_string [white; on_red] (" 3  "); print_topline xs
    | Some tile, _ -> print_string [] ("|"); print_string [Bold; white; on_black] (" " ^ tile.letter ^ "  "); print_topline xs

(** [print_topline line] prints the bottom half of [line], where [line] is one
    row of a board *)
let rec print_botline line =
  match line with
  | [] -> print_endline "|"
  | x::xs -> match x with
<<<<<<< HEAD
    | None, NaN -> print_string [] "|    "; print_botline xs
    | None, DoubleLetter -> print_string [] ("|"); print_string [white; on_cyan] ("  L "); print_botline xs
    | None, TripleLetter -> print_string [] ("|"); print_string [white; on_blue] ("  L "); print_botline xs
    | None, DoubleWord -> print_string [] ("|"); print_string [white; on_magenta] ("  W "); print_botline xs
    | None, TripleWord -> print_string [] ("|"); print_string [white; on_red] ("  W "); print_botline xs
    | Some tile, _ -> print_string [] ("|"); print_string [Bold; white; on_black] ("  " ^ string_of_int tile.value ^ " "); print_botline xs
=======
    | None, NaN -> print_string [] "|  "
    | None, DoubleLetter -> print_string [on_cyan] ("| L")
    | None, TripleLetter -> print_string [on_blue] ("| L")
    | None, DoubleWord -> print_string [on_magenta] ("| W")
    | None, TripleWord -> print_string [on_red] ("| W")
    | Some tile, _ -> print_string [] ("| " ^ string_of_int tile.value) 
>>>>>>> f505eeab34163599127d3c23654dfeebb3ae827f

let print_linenum i =
  print_string [] (string_of_int i)

(** [print_board board] prints a graphical representation of [board] into the
    terminal window *)
let rec print_board board i =
  print_endline "+————+————+————+————+————+————+————+————+————+————+————+————+————+————+————+";
  match board with
<<<<<<< HEAD
  | [] ->  print_endline "";
  | x::xs -> print_topline x; print_botline x; print_board xs


=======
  | [] ->  print_endline " 0    1    2    3    4    5    6    7    8    9    10   11   12   13   14";
  | x::xs -> print_linenum i; print_topline x; print_botline x; print_board xs (i + 1)
>>>>>>> 5e2701f90e5980c65dbb3b19f4255b9f81615daa
