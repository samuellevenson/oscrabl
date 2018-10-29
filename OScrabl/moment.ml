open Board
open ANSITerminal
open Actions

(** the exception raised when there is an attempt to draw from an empty bag *)
exception EmptyBag

(** the type of the player *)
type player = {
  name: string;
  dock: pretile list;
  score: int;
  words: string list
}

(** the type of the moment (state) *)
type t = {
  board: board;
  bag: pretile list;
  players: player list;
  current_player: player;
}

(** [rand_num] is a random integer of 30 bits *)
let rand_num = (Random.self_init ()); Random.bits

(** [shuffle_bag bag] is [bag] with its contents in a random order *)
let shuffle_bag bag =
  bag
  |> List.map (fun n -> ((rand_num ()) * rand_num()), n)
  |> List.sort compare
  |> List.map snd

(* creates a bag of tiles containing the distribution of 100 scrabble tiles in
   a random order *)
let init_bag =
  shuffle_bag [
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

(** [draw currentBag] draws one tile from the given bag and returns a tuple
    containing the drawn tile and a bag with the remaining tiles *)
let draw currentBag: (pretile * pretile list) =
  if (List.length currentBag) = 0 then raise EmptyBag
  else begin let rec helper bg acclist =
               match bg with
               | [] -> List.rev acclist
               | h::t -> if (List.length bg = List.length currentBag) then helper t acclist
                 else (helper t) (h::acclist)
    in ((List.hd currentBag), helper currentBag []) end

(** [draw_n_times currentBag n] draws [n] tiles from the given bag and returns a
    tuple containing a list of the drawn tiles and a bag with the remaining
    tiles *)
let draw_n_times currentBag n: (pretile list * pretile list) =
  let rec helper bg n accTileList =
    if (n>0) then match (draw bg) with
      | (ti, ba) ->  helper ba (n-1) (ti::accTileList)
    else (accTileList, bg) in
  helper currentBag n []

(** [init_draw_num] is the number of tiles to draw from the bag at the start of
    the game *)
let init_draw_num num_of_players = num_of_players * 7

(** [init_state] creates a state with an empty board, a player with 7 random
    tiles drawn from a bag, and a bag with all tiles except for those 7 *)
let init_state = {
  board = emptyBoard;
  bag = (match (draw_n_times init_bag (init_draw_num 1)) with
      | (tilelist, bg) -> bg);
  players = [
    {
      name = "OScrabl Player";
      dock =
        (match (draw_n_times init_bag (init_draw_num 1)) with
         | (tilelist, bg) -> tilelist);
      score = 0;
      words = [];
    }
  ];
  current_player =  {
    name = "OScrabl Player";
    dock =
      (match (draw_n_times init_bag (init_draw_num 1)) with
       | (tilelist, bg) -> tilelist);
    score = 0;
    words = [];
  };
}

(** [letter_to_tile] is a function taking a string containing a single letter
    and a dock as input, then returns the tile containing the letter in the dock
    if it exists.**)
let letter_to_tile letter state =
  let rec dock_iter = function
    | [] -> raise BadSelection
    | h::t -> if letter = h.letter then h else (dock_iter t)
  in dock_iter state.current_player.dock

(* alternate using List module function *)
let letter_to_tile letter state =
  try List.find (fun x -> x.letter = letter) state.current_player.dock
  with
  | _ -> raise BadSelection

(** [remove_tile_from_dock state tile] returns the state in which [tile] has
    been removed from the current players dock in [state] *)
let remove_tile_from_dock state tile =
  let rec dock_iter tile dock acc =
    match dock with
    | [] -> List.rev acc
    | x::xs -> if x = tile then (acc@xs)
      else (dock_iter tile xs (acc@[x])) in
  {
    name = state.current_player.name;
    score = state.current_player.score;
    words = state.current_player.words;
    dock = dock_iter tile state.current_player.dock []
  }

(** [update_player]  player list -> player -> player list  -> player list
    takes a given player list and updated player, and replaces the older version
    of that player with the updated version in the list.*)
let rec update_player player_list player acc =
  match player_list with
  | [] -> acc
  | h::t -> if h.name = player.name then update_player t player (player::acc)
    else update_player t player acc

(** [update_player_in_list] takes the current state and player and updates
    the state's player list with the new player instance. **)
let update_player_in_list st player =
  {
    board = st.board;
    bag = st.bag;
    players = (update_player st.players player []);
    current_player = st.current_player;
  }

(** [refill] t -> t
    Refill takes in a state and then returns a state in which the current player's
    dock has been refilled to 7 tiles. *)
let refill state = 
  let tiles_to_draw =  7 - (List.length state.current_player.dock) in
  let list_bag_tuple = draw_n_times state.bag tiles_to_draw in 
  let updated_current_player = 
    {
      name = state.current_player.name;
      dock = (fst list_bag_tuple) @ state.current_player.dock;
      score = state.current_player.score;
      words = state.current_player.words;
    } in
  {
    board = state.board;
    bag = snd list_bag_tuple;
    players = (update_player state.players updated_current_player []);
    current_player = updated_current_player;
  }


(** TODO: all the stuff that happens when a player ends their turn *)
let end_turn state =
  failwith "TODO"

(** finds the position of some unfinal tile on the board, returns it as (x,y) *)
let rec find_unfinal board: (int * int) =
  let rec board_iter x y =
    match fst (get_square board (x, y)) with
    | Unfinal tile -> (x,y)
    | _ ->
      if x < 15 then (board_iter (x+1) y)
      else if y < 15 then (board_iter 0 (y+1))
      else failwith "no unfinal tiles on this board"
  in board_iter 0 0

(** returns true if all squares outside of the cross centered on (x,y) do not
    contain unfinal tiles *)
let check_uncrossed board (x_fix, y_fix) =
  let rec board_iter x y =
    match fst (get_square board (x, y)) with
    | Unfinal tile ->
      if x <> x_fix && y <> y_fix then false
      else if x < 15 then board_iter (x+1) y
      else if y < 15 then board_iter 0 (y+1)
      else true
    | _ ->
      if x < 15 then board_iter (x+1) y
      else if y < 15 then board_iter 0 (y+1)
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

(** [place_tile state (letter,(row,col))] is the new state after a tile
    corresponding to [letter] has been taken from the current player's dock and
    placed onto the board at the position specified by (row,col). Raises
    BadSelection if there is no tile in the player's dock of that letter. *)
let place_tile state (letter,(row,col)) =
  let tile = letter_to_tile letter state in
  {
    board = insertTile state.board (Unfinal tile) (row,col);
    bag = state.bag;
    players = state.players;
    current_player = remove_tile_from_dock state tile
  }

(** [print_docktop dock] prints the top line of a players dock of tiles *)
let rec print_docktop dock =
  match dock with
  | [] -> print_endline ""
  | x::xs ->
    print_string tile_style (" " ^ x.letter ^ "  ");
    print_string [] "  "; print_docktop xs

(** [offset tile] is the spaces needed after the value of a tile in order to
    account for differences in number of digits.*)
let offset tile =
  if tile.value >= 10 then "" else " "

(** [print_dockbot dock] prints the bottom line a players dock of tiles *)
let rec print_dockbot dock =
  match dock with
  | [] -> print_endline ""
  | x::xs ->
    print_string tile_style ("  " ^ string_of_int x.value ^ offset x);
    print_string [] "  "; print_dockbot xs

(** [print_dock player] prints all of the tiles in a players docks *)
let rec print_dock player =
  let dock = player.dock in
  print_string [] "                  "; print_docktop dock;
  print_string [] "                  "; print_dockbot dock

(** [print_game st] prints the board and dock of the state [st] *)
let print_game st =
  print_board (st.board) 0;
  print_endline "";
  print_dock (st.current_player);
  print_endline "";
