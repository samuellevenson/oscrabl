open Board
open ANSITerminal
open Actions

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

let init_bag = [
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
]

let init_state = {
  board = emptyBoard;
  bag = init_bag;
  players = [
    {name = "OScrabl Player";
     dock = [
       {letter = "A"; value = 1};
       {letter = "B"; value = 3};
       {letter = "C"; value = 3};
       {letter = "D"; value = 2};
       {letter = "E"; value = 1};
       {letter = "F"; value = 4};
       {letter = "G"; value = 2};
     ];
     score = 0;
     words = [];
    }
  ];
  current_player = {name = "OScrabl Player";
                    dock = [
                      {letter = "A"; value = 1};
                      {letter = "B"; value = 3};
                      {letter = "C"; value = 3};
                      {letter = "D"; value = 2};
                      {letter = "E"; value = 1};
                      {letter = "F"; value = 4};
                      {letter = "G"; value = 2};
                    ];
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

let remove_tile_from_dock player tile =
  {
    name = player.name;
    score = player.score;
    words = player.words;
    dock = List.filter (fun x -> tile <> x) player.dock
  }

let update_state st cmd =
  match cmd with
  | Place (letter,(row,col)) -> begin
      let tile = letter_to_tile letter st.current_player.dock in
      match tile with
      | tile -> let updated_board = insertTile st.board (Some tile) (row,col) in
        {board = updated_board; bag = st.bag; players = st.players;
         current_player = remove_tile_from_dock st.current_player tile}
    end
  | Score -> failwith ""
  | Quit -> failwith ""

let rec draw state : t =
  if List.length state.current_player.dock = 7 then state
  else let next_state = {
    board = state.board;
    players = state.players;
    current_player = {
      name = state.current_player.name;
      score = state.current_player.score;
      words = state.current_player.words;
      dock = List.hd state.bag::state.current_player.dock
    };
    bag = List.tl state.bag
  } in
    draw next_state

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
