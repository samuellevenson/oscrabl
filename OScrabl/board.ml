type tile = {
  letter: string;
  value: int;
}

type multiplier = Letter of int | Word of int

type square = tile option * multiplier option

type board = square list list

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
