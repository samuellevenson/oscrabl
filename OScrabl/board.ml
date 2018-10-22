
type tile = {
  letter: string; value: int
}

type multiplier = Letter of int | Word of int | NaN

type square = (tile option) * (multiplier)

type board = square list list

let def_col_zero : square list = 
  let rec helper row acclist = 
    if (row < 15) then 
      begin 
        if (row = 0) then helper (row + 1) ((None, Word(3))::acclist)
        else if (row <> 0) && (row < 3) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 3) then helper (row + 1) ((None, Letter(2))::acclist)
        else if (row <> 3) && (row < 7) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 7) then helper (row + 1) ((None, Word(3))::acclist)
        else if (row <> 7) && (row < 11) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 11) then helper (row + 1) ((None, Letter(2))::acclist)
        else if (row <> 11) && (row < 14) then helper (row + 1) acclist
        else helper (row + 1) ((None, Word(3))::acclist) 
      end
    else List.rev acclist
  in helper 0 []

let def_col_one =
  let rec helper row acclist = 
    if (row < 15) then 
      begin
        if (row = 0) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 1) then helper (row + 1) ((None, Word(2))::acclist)
        else if (row < 5) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 5) then helper (row + 1) ((None, Letter(3))::acclist)
        else if (row < 9) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 9) then helper (row + 1) ((None, Letter(3))::acclist)
        else if (row < 13) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 13) then helper (row + 1) ((None, Word(2))::acclist)
        else helper (row + 1) ((None, NaN)::acclist)
      end
    else List.rev acclist 
  in helper 0 []

let def_col_two = 
  let rec helper row acclist = 
    if (row < 15) then 
      begin
        if (row < 2) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 2) then helper (row + 1) ((None, Word(2))::acclist)
        else if (row < 6) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 6) then helper (row + 1) ((None, Letter(2))::acclist)
        else if (row < 8) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 8) then helper (row + 1) ((None, Letter(2))::acclist)
        else if (row < 12) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 12) then helper (row + 1) ((None, Word(2))::acclist)
        else helper (row + 1) ((None, NaN)::acclist)
      end
    else List.rev acclist 
  in helper 0 []

let def_col_three = 
  let rec helper row acclist = 
    if (row < 15) then 
      begin
        if (row = 0) then helper (row + 1) ((None, Letter(2))::acclist)
        else if (row < 3) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 3) then helper (row + 1) ((None, Word(2))::acclist)
        else if (row < 7) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 7) then helper (row + 1) ((None, Letter(2))::acclist)
        else if (row < 11) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 11) then helper (row + 1) ((None, Word(2))::acclist)
        else if (row < 14) then helper (row + 1) ((None, NaN)::acclist)
        else helper (row + 1) ((None, Letter(2))::acclist)
      end
    else List.rev acclist 
  in helper 0 []

let def_col_four = 
  let rec helper row acclist = 
    if (row < 15) then 
      begin
        if (row < 4) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 4) then helper (row + 1) ((None, Word(2))::acclist)
        else if (row < 10) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 10) then helper (row + 1) ((None, Word(2))::acclist)
        else helper (row + 1) ((None, NaN)::acclist)
      end
    else List.rev acclist 
  in helper 0 []

let def_col_five = 
  let rec helper row acclist = 
    if (row < 15) then 
      begin
        if (row = 0) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 1) then helper (row + 1) ((None, Letter(3))::acclist)
        else if (row < 5) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 5) then helper (row + 1) ((None, Letter(3))::acclist)
        else if (row < 9) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 9) then helper (row + 1) ((None, Letter(3))::acclist)
        else if (row < 13) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 13) then helper (row + 1) ((None, Letter(3))::acclist)
        else helper (row + 1) ((None, NaN)::acclist)
      end
    else List.rev acclist 
  in helper 0 []

let def_col_six = 
  let rec helper row acclist = 
    if (row < 15) then 
      begin
        if (row < 2) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 2) then helper (row + 1) ((None, Letter(2))::acclist)
        else if (row < 6) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 6) then helper (row + 1) ((None, Letter(2))::acclist)
        else if (row < 8) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 8) then helper (row + 1) ((None, Letter(2))::acclist)
        else if (row < 12) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 12) then helper (row + 1) ((None, Letter(2))::acclist)
        else helper (row + 1) ((None, NaN)::acclist)
      end
    else List.rev acclist 
  in helper 0 []

let def_col_seven = 
  let rec helper row acclist = 
    if (row < 15) then 
      begin
        if (row = 0) then helper (row + 1) ((None, Word(3))::acclist)
        else if (row < 3) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 3) then helper (row + 1) ((None, Letter(2))::acclist)
        else if (row < 7) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 7) then helper (row + 1) ((None, Word(2))::acclist)
        else if (row < 11) then helper (row + 1) ((None, NaN)::acclist)
        else if (row = 11) then helper (row + 1) ((None, Letter(2))::acclist)
        else if (row < 14) then helper (row + 1) ((None, NaN)::acclist)
        else helper (row + 1) ((None, Word(3))::acclist)
      end
    else List.rev acclist 
  in helper 0 []

let emptyBoard = 
  [def_col_one;def_col_two;def_col_three;def_col_four;def_col_five;
   def_col_six;def_col_seven;def_col_six;def_col_five;def_col_four;
   def_col_three;def_col_two;def_col_one;def_col_zero]

let insertTile board tile (x,y)= 
  let rec rowIter row col new_col = 
    if row < x then rowIter (row + 1) col ((List.nth col row)::new_col)
    else if row = x then 
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

let rec print_topline line =
  match line with
  | [] -> print_string "|"
  | x::xs -> match x with
    | None, NaN -> print_string "|  |"
    | None, Letter x -> print_string ("|" ^ string_of_int x ^ " ")
    | None, Word x -> print_string ("|" ^ string_of_int x ^ " ")
    | Some tile, _ -> print_string ("|" ^ tile.letter ^ " ")

let rec print_botline line =
  match line with
  | [] -> print_string "|"
  | x::xs -> match x with
    | None, NaN -> print_string "|  "
    | None, Letter x -> print_string ("| L")
    | None, Word x -> print_string ("| W")
    | Some tile, _ -> print_string ("| " ^ string_of_int tile.value)

let rec print_board board =
  print_endline "+——+——+——+——+——+——+——+——+——+——+——+——+——+——+——+——+——+";
  match board with
  | [] ->  print_endline "+——+——+——+——+——+——+——+——+——+——+——+——+——+——+——+——+——+";
  | x::xs -> print_topline x; print_botline x; print_board xs
