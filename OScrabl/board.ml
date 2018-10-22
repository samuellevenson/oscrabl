type tile = {
  letter: string;
  value: int;
}

type multiplier = Letter of int | Word of int

type square = tile option * multiplier option

type board = square list list

let default_column_one = 
  let rec helper row acclist = 
    if (row < 15) then 
      begin
        if (row = 0) then helper (row + 1) (Word(3)::acclist)
        else if (row <> 0) && (row < 3) then helper (row+1) acclist
        else if (row = 3) then helper (row + 1) (Letter(2)::acclist)
        else if (row <> 3) && (row < 7) then helper (row + 1) acclist 
        else if (row = 7) then helper (row + 1) (Word(3)::acclist)
        else if (row <> 7) && (row < 11) then helper (row + 1) acclist
        else if (row = 11) then helper (row + 1) (Letter(2)::acclist)
        else if (row <> 11) && (row < 14) then helper (row + 1) acclist
        else helper (row + 1) (Word(3)::acclist) 
      end
    else List.rev acclist 
  in helper 0 []

let default_column_two = 
  let rec helper row acclist = 
    if (row < 15) then 
      begin
        if (row = 0) then helper (row + 1) (Word(3)::acclist)
        else if (row <> 0) && (row < 3) then helper (row+1) acclist
        else if (row = 3) then helper (row + 1) (Letter(2)::acclist)
        else if (row <> 3) && (row < 7) then helper (row + 1) acclist 
        else if (row = 7) then helper (row + 1) (Word(3)::acclist)
        else if (row <> 7) && (row < 11) then helper (row + 1) acclist
        else if (row = 11) then helper (row + 1) (Letter(2)::acclist)
        else if (row <> 11) && (row < 14) then helper (row + 1) acclist
        else helper (row + 1) (Word(3)::acclist) 
      end
    else List.rev acclist 
  in helper 0 []

let emptyBoard = 
  let col = 0 in 
  let row = 0 in 
  let rec helperCol col accList= 
    if (col )

let insertTile board tile (x,y)= 
  let col = 0 in
  let rec columnIter col list_of_columns = 
    if col = 0 then 

      let rec print_topline line =
        match line with
        | [] -> print_string "|"
        | x::xs -> match x with
          | None, None -> print_string "|  |"
          | None, Some Letter x -> print_string ("|" ^ string_of_int x ^ " ")
          | None, Some Word x -> print_string ("|" ^ string_of_int x ^ " ")
          | Some tile, _ -> print_string ("|" ^ tile.letter ^ " ")

let rec print_botline line =
  match line with
  | [] -> print_string "|"
  | x::xs -> match x with
    | None, None -> print_string "|  "
    | None, Some Letter x -> print_string ("| L")
    | None, Some Word x -> print_string ("| W")
    | Some tile, _ -> print_string ("| " ^ string_of_int tile.value)

let rec print_board board =
  print_endline "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+";
  match board with
  | [] ->  print_endline "+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+";
  | x::xs -> print_topline x; print_botline x; print_board xs
