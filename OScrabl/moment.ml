open Board
open ANSITerminal
open Actions

(** the exception raised when there is an attempt to draw from an empty bag *)
exception EmptyBag

(** the type of the player *)
type player = {
  name: string;
  dock: tile list;
  score: int;
  words: string list
}

(** the type of the moment (state) *)
type t = {
  board: board;
  bag: tile list;
  players: player list;
  current_player: player;
}

(** [rand_num] is a random integer of 30 bits *)
let rand_num = (Random.self_init ()); Random.bits

(* creates a bag of tiles containing the distribution of 100 scrabble tiles in
   a random order *)
let init_bag =
  let init_bag_unmixed = [
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
  in let l1 = List.map (fun n -> ((rand_num ()) * rand_num()), n)
         init_bag_unmixed
  in let l2 = List.sort compare l1
  in List.map snd l2

(** [draw currentBag] draws one tile from the given bag and returns a tuple
    containing the drawn tile and a bag with the remaining tiles *)
let draw currentBag: (tile * tile list) =
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
let draw_n_times currentBag n: (tile list * tile list) =
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

(** [update_player_in_list] takes the current state and player and updates
    the state's player list with the new player instance. **)
let update_player_in_list st player =
  let rec update_player player_list player acc =
    match player_list with
    | [] -> acc
    | h::t -> if h.name = player.name then update_player t player (player::acc)
      else update_player t player acc
  in
  {
    board = st.board;
    bag = st.bag;
    players = (update_player st.players player []);
    current_player = st.current_player;
  }

(** [updated_state st cmd] is the new state of the game after applying the
    command [cmd] to the old state [st] *)
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

(** unused *)
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

(** [print_docktop dock] prints the top line of a players dock of tiles *)
let rec print_docktop dock =
  match dock with
  | [] -> print_endline ""
  | x::xs -> print_string [Bold; white; on_black] (" " ^ x.letter ^ "  "); print_string [] "  "; print_docktop xs

(** [offset tile] is the spaces needed after the value of a tile in order to
    account for differences in number of digits.*)
let offset tile =
  if tile.value >= 10 then "" else " "

(** [print_dockbot dock] prints the bottom line a players dock of tiles *)
let rec print_dockbot dock =
  match dock with
  | [] -> print_endline ""
  | x::xs -> print_string [Bold; white; on_black] ("  " ^ string_of_int x.value ^ offset x); print_string [] "  "; print_dockbot xs

(** [print_dock player] prints all of the tiles in a players docks *)
let rec print_dock player =
  let dock = player.dock in
  print_string [] "                    "; print_docktop dock;
  print_string [] "                    "; print_dockbot dock

(** [print_game st] prints the board and dock of the state [st] *)
let print_game st =
  print_board (st.board) 0;
  print_endline "";
  print_dock (st.current_player)
