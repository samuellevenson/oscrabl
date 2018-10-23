open Board
open ANSITerminal
open Actions

exception EmptyBag 

type player = {
  name: string;
  dock: tile list;
  score: int;
  words: string list
}

type t = {
  board: board;
  bag: tile list;
  players: player list;
  current_player: player;
}

(* this is randomized *)
let init_bag = 
  let init_bag_contents = [
    {letter = "_"; value = 0};
    {letter = "_"; value = 0};
    {letter = "A"; value = 1};
    {letter = "A"; value = 1};
    {letter = "A"; value = 1};
    {letter = "A"; value = 1};
    {letter = "A"; value = 1};
    {letter = "A"; value = 1};
    {letter = "A"; value = 1};
    {letter = "A"; value = 1};
    {letter = "A"; value = 1};
    {letter = "B"; value = 3};
    {letter = "B"; value = 3};
    {letter = "C"; value = 3};
    {letter = "C"; value = 3};
    {letter = "D"; value = 2};
    {letter = "D"; value = 2};
    {letter = "D"; value = 2};
    {letter = "D"; value = 2};
    {letter = "E"; value = 1};
    {letter = "E"; value = 1};
    {letter = "E"; value = 1};
    {letter = "E"; value = 1};
    {letter = "E"; value = 1};
    {letter = "E"; value = 1};
    {letter = "E"; value = 1};
    {letter = "E"; value = 1};
    {letter = "E"; value = 1};
    {letter = "E"; value = 1};
    {letter = "E"; value = 1};
    {letter = "E"; value = 1};
    {letter = "F"; value = 4};
    {letter = "F"; value = 4};
    {letter = "G"; value = 2};
    {letter = "G"; value = 2};
    {letter = "G"; value = 2};
    {letter = "H"; value = 4};
    {letter = "H"; value = 4};
    {letter = "I"; value = 1};
    {letter = "I"; value = 1};
    {letter = "I"; value = 1};
    {letter = "I"; value = 1};
    {letter = "I"; value = 1};
    {letter = "I"; value = 1};
    {letter = "I"; value = 1};
    {letter = "I"; value = 1};
    {letter = "I"; value = 1};
    {letter = "J"; value = 8};
    {letter = "K"; value = 5};
    {letter = "L"; value = 1};
    {letter = "L"; value = 1};
    {letter = "L"; value = 1};
    {letter = "L"; value = 1};
    {letter = "M"; value = 3};
    {letter = "M"; value = 3};
    {letter = "N"; value = 1};
    {letter = "N"; value = 1};
    {letter = "N"; value = 1};
    {letter = "N"; value = 1};
    {letter = "N"; value = 1};
    {letter = "N"; value = 1};
    {letter = "O"; value = 1};
    {letter = "O"; value = 1};
    {letter = "O"; value = 1};
    {letter = "O"; value = 1};
    {letter = "O"; value = 1};
    {letter = "O"; value = 1};
    {letter = "O"; value = 1};
    {letter = "P"; value = 3};
    {letter = "P"; value = 3};
    {letter = "Q"; value = 10};
    {letter = "R"; value = 1};
    {letter = "R"; value = 1};
    {letter = "R"; value = 1};
    {letter = "R"; value = 1};
    {letter = "R"; value = 1};
    {letter = "R"; value = 1};
    {letter = "S"; value = 1};
    {letter = "S"; value = 1};
    {letter = "S"; value = 1};
    {letter = "S"; value = 1};
    {letter = "T"; value = 1};
    {letter = "T"; value = 1};
    {letter = "T"; value = 1};
    {letter = "T"; value = 1};
    {letter = "T"; value = 1};
    {letter = "T"; value = 1};
    {letter = "U"; value = 1};
    {letter = "U"; value = 1};
    {letter = "U"; value = 1};
    {letter = "U"; value = 1};
    {letter = "V"; value = 4};
    {letter = "V"; value = 4};
    {letter = "W"; value = 4};
    {letter = "W"; value = 4};
    {letter = "X"; value = 8};
    {letter = "Y"; value = 4};
    {letter = "Y"; value = 4};
    {letter = "Z"; value = 10};
  ] in 
  let tagged_list = List.map (fun t -> (Random.bits, t)) init_bag_contents in 
  let sorted_tagged_list = 
    List.sort (fun (n1, t1) (n2, t2) -> (compare n1 n2)) tagged_list in 
  List.map (fun (n,t) -> n) sorted_tagged_list

let draw currentBag: (tile * bag) = 
  if (List.length currentBag) = 0 then raise EmptyBag
  else let rec helper bg acclist = 
         match bg with 
         | [] -> List.rev acclist
         | h::t -> if (List.length bg = List.length currentBag) then helper t acclist
           else (helper t h)::acclist
    in ((List.hd currentBag), helper currentBag [])

let draw_n_times currentBag n: (tile list * bag) = 
  let rec helper bg n accTileList = 
    if (n>0) then match (draw bg) with 
      | (ti, ba) ->  helper ba (n-1) ti::accTileList
    else (accTileList, bg) in 
  helper currentBag n []

let init_draw_num num_of_players = num_of_players * 7

let init_state = {
  board = emptyBoard;
  bag = match (draw_n_times init_bag (init_draw_num 1)) with 
    | (tilelist, bg) -> bg;
      players = [
        {name = "OScrabl Player";
         dock = match (draw_n_times init_bag (init_draw_num 1)) with 
           | (tilelist, bg) -> tilelist;
             score = 0;
             words = [];
        }
      ];
      current_player =  {name = "OScrabl Player";
                         dock = match (draw_n_times init_bag (init_draw_num 1)) with 
                           | (tilelist, bg) -> tilelist;
                             score = 0;
                             words = [];
                        };
}

let update_board board tile (x,y) =
  insertTile board tile (x,y)

(* FIX update_dock *)
(* let update_dock = failwith "update_dock" *)

(** [letter_to_tile] is a function taking a string containing a single letter
    and a dock as input, then returns the tile containing the letter in the dock
    if it exists.**)
let rec letter_to_tile letter dock =
  match dock with
  | [] -> raise BadSelection
  | h::t -> if letter = h.letter then h else (letter_to_tile letter t)

let update_state st cmd =
  match cmd with
  | Place (letter,(row,col)) ->
    let tile = letter_to_tile letter st.current_player.dock in
    match tile with
    | tile -> let updated_board = insertTile st.board (Some tile) (row,col) in
      {board = updated_board; bag = st.bag; players = st.players;
       current_player = st.current_player}

let rec print_docktop dock =
  match dock with
  | [] -> print_endline ""
  | x::xs -> print_string [Bold; white; on_black] (" " ^ x.letter ^ "  "); print_string [] "  "; print_docktop xs

let rec print_dockbot dock =
  match dock with
  | [] -> print_endline ""
  | x::xs -> print_string [Bold; white; on_black] ("  " ^ string_of_int x.value ^ " "); print_string [] "  "; print_dockbot xs

let rec print_dock player =
  let dock = player.dock in
  print_string [] "                    "; print_docktop dock;
  print_string [] "                    "; print_dockbot dock

let print_game st =
  print_board (st.board) 0;
  print_endline "";
  print_dock (st.current_player)
