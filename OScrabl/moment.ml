open Board
open ANSITerminal
open Actions

(** the exception raised when there is an attempt to draw from an empty bag *)
exception EmptyBag
exception InvalidExchange

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
        (match (draw_n_times (shuffle_bag (shuffle_bag init_bag)) (init_draw_num 1)) with
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

(** [generate_initial_state] player list -> t
    creates an initial game state given the list of players.*)
let generate_initial_state player_list = {
  board = emptyBoard;
  bag = (match (draw_n_times init_bag (init_draw_num 2)) with
      | (tilelist, bg) -> bg);
  players = player_list;
  current_player =  List.hd player_list;
}

(** [generate_player] string -> player
    Creates a player given a string containing a name as input.*)
let generate_player nm st =
  {
    name = nm;
    dock =
      (match (draw_n_times st.bag 7) with
       | (tilelist, bg) -> tilelist);
    score = 0;
    words = [];
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

(** turn ending implemented for 1 player game *)
let end_turn state : (t * string) =
  let draw_num = 7 - List.length state.current_player.dock in
  let score = calc_score state.board in
  let curr_player = {
    name = state.current_player.name;
    dock = state.current_player.dock @ fst (draw_n_times state.bag draw_num);
    score = state.current_player.score + score;
    words = state.current_player.words
  } in
  ({
    board = finalize state.board;
    bag = snd (draw_n_times state.bag draw_num);
    players = List.rev (curr_player::(List.tl (state.players))) ;
    current_player =
      if List.length state.players > 1 then List.hd (List.tl state.players)
      else curr_player;
  }, string_of_int score)

(** [refill] t -> t
    Refill takes in a state and then returns a state in which the current player's
    dock has been refilled to 7 tiles. *)
let refill state =
  let tiles_to_draw =  (7 - (List.length state.current_player.dock)) in
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

(** [refill]_set_num t -> t
    [refill_set_num] is [refill], but with the number of tiles to draw specified.
*)
let refill_set_num state num =
  let tiles_to_draw =  num in
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

(** [pretile_to_string] pretile list -> string list -> string list
    is a function that takes a list of pretiles and returns a string list where
    each element is the character string associated with the pretile.*)
let rec pretile_to_string (pretile_lst: pretile list) : string list =
  List.map (fun x -> x.letter) pretile_lst

(** [remove_first_instance c l acc] is [l] with the first instance of [c]
    removed. Requires: [acc] is the emtpy list. *)
let rec remove_first_instance to_check lst acc =
  match lst with
  | [] -> List.rev acc
  | h::t -> if h = to_check then (acc@t)
    else (remove_first_instance to_check t (acc@[h]))

(** [check_tiles_are_valid] t -> string list -> bool
    is a function that checks whether all strings in a string list are in the
    current player's dock. *)
let check_tiles_are_valid (state:t) (lst: string list) : bool =
  let rec check_tiles_are_valid_helper (str_lst:string list)
      (pretile_to_string_lst:string list) =
    match (str_lst,pretile_to_string_lst) with
    | [],_::_ -> true
    | _::_, [] -> false
    | [], [] -> true
    | (h::t,dock) -> if List.mem h pretile_to_string_lst
      then check_tiles_are_valid_helper t (remove_first_instance h dock [])
      else false
  in check_tiles_are_valid_helper lst (pretile_to_string (state.current_player.dock) )

(**[return_to_dock st tiles] is the game state [st] with [tiles] appended to the
   current player's dock *)
let return_to_dock st tiles : t =
  let new_dock = (st.current_player.dock@tiles) in
  let new_curplayer = {name = st.current_player.name; dock = new_dock;
                       score = st.current_player.score; words = st.current_player.words} in
  {board = st.board; bag = st.bag; players = st.players;
   current_player = new_curplayer}

(**[recall st] is the updated [st] after Unfinal tiles are recalled. *)
let recall st =
  let board_and_pretiles = pop_unfinals st.board in
  let new_dockstate = return_to_dock st (snd board_and_pretiles) in
  {board = (fst board_and_pretiles); bag = new_dockstate.bag;
   players = new_dockstate.players;
   current_player = new_dockstate.current_player}


(** [exchange] t -> string list -> t
    takes in the current game state and a string list from user input and
    removes them from the dock, then refills the dock,
    effectively "exchanging" the tiles.*)
let exchange state lst =
  if (check_tiles_are_valid state lst) &&
     (List.length state.current_player.dock = 7) then
    (*get the remaining letters in the dock after removing them. *)
    let rec exchange_helper (dock:Board.pretile list) (str_lst:string list) acc=
      let upper_str_lst = to_upper_case str_lst in
      match dock with
      | [] ->  acc
      | h::t -> if (List.mem (h.letter) upper_str_lst) then
          (exchange_helper t (remove_first_instance h.letter upper_str_lst []) acc)
        else (exchange_helper t upper_str_lst (h::acc))
    in
    let new_state = {
      board = state.board;
      bag = state.bag;
      players = state.players;
      current_player = {
        name = state.current_player.name;
        dock = List.rev (exchange_helper state.current_player.dock lst []);
        score = state.current_player.score;
        words = state.current_player.words;
      };
    }
    in refill_set_num new_state (List.length lst)
  else raise InvalidExchange

(** [place_tile state (letter,(row,col))] is the new state after a tile
    corresponding to [letter] has been taken from the current player's dock and
    placed onto the board at the position specified by (row,col). Raises
    BadSelection if there is no tile in the player's dock of that letter. *)
let place_tile state (letter,pos) =
  let tile = letter_to_tile letter state in
  {
    board = insertTile state.board (Unfinal tile) pos;
    bag = state.bag;
    players = state.players;
    current_player = remove_tile_from_dock state tile;
  }

(** TODO: docs *)
let pickup_tile state pos : (t * string) =
  let (new_board, tile) = (remove_tile state.board pos) in
  ({
    board = new_board;
    bag = state.bag;
    players = state.players;
    current_player = let p = state.current_player in
      {
        name = p.name;
        score = p.score;
        words = p.words;
        dock = tile::p.dock;
      }
  }, tile.letter)

(** TODO: docs *)
let get_score state =
  state.current_player.score |> string_of_int

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
