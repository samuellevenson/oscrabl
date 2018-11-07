(** Representation of the gamestate at a particular moment *)

open Board
open ANSITerminal
open Actions
open Yojson.Basic

(** the exception raised when there is an attempt to draw from an empty bag *)
exception EmptyBag
exception MissingTilesToExchange
exception InvalidExchange
exception BagTooSmall

(** the type of the player *)
type player = {
  name: string;
  dock: pretile list;
  score: int;
}

(** the type of the moment *)
type t = {
  board: board;
  bag: pretile list;
  players: player list;
  current_player: player;
  log: string list;
  scoreless: int
}

let create_player pname pdock pscore =
  { name = pname;
    dock = pdock;
    score = pscore;}

let create_moment board bag players current_player log =
  {
    board = board;
    bag = bag;
    players = players;
    current_player = current_player;
    log = log;
    scoreless = 0;
  }

(** [get_board] t -> Board.board
    Returns the board of the current game state. *)
let get_board st = st.board

(** [get_name] player -> string
    Returns the name of the given player. *)
let get_name player = player.name

(* [get_player_score] player -> int
   Returns the score of the given player.*)
let get_player_score player = player.score

(** [get_current_player] t -> player
    Returns the currently active player in the game state. *)
let get_current_player st = st.current_player

(** [get_other_player] t -> player
    Is the non-active player.*)
let get_other_player st =
  List.hd (List.tl st.players)

(** [get_dock] player -> Board.pretile list
    is the dock of a selected player*)
let get_dock player = player.dock

(** [get_current_dock] t -> Board.pretile list
    is the dock of the current player. *)
let get_current_dock st = (get_current_player st).dock

(** [rand_num] is a random integer of 30 bits *)
let rand_num = (Random.self_init ()); Random.bits

(** [shuffle_bag bag] is [bag] with its contents in a random order *)
let shuffle_bag bag =
  bag
  |> List.map (fun n -> ((rand_num ()) * rand_num()), n)
  |> List.sort compare
  |> List.map snd

(** [make_tile tile] creates the pretile record type from a json object [tile]*)
let make_tile (tile:json) : pretile = {
  letter = tile |> Util.member "letter" |> Util.to_string;
  value = tile |> Util.member "value" |> Util.to_int;
}

(** [tiles_from_json] reads the contents "tiles.json" in order to create a list
    of pretiles that will be the initial contents of the bag *)
let tiles_from_json : pretile list =
  Yojson.Basic.from_file "tiles.json"|> Util.to_list |> List.map make_tile

(* creates a bag of tiles containing the distribution of 100 scrabble tiles in
   a random order *)
let init_bag =
  shuffle_bag tiles_from_json

(** [draw_n start_bag start_n] is the previous two functions simplified and then
    squished into one function *)
let draw_n start_bag start_n =
  let rec repeat_draw bag n tiles_drawn =
    match bag with
    | [] when n > 0 -> raise EmptyBag
    | [] when n = 0 -> (tiles_drawn, bag)
    | x::xs when n > 0 -> repeat_draw xs (n-1) (x::tiles_drawn)
    | x::xs when n = 0 -> (tiles_drawn, bag)
    | _ -> failwith "start_n < 0"
  in repeat_draw start_bag start_n []

(** shift log if length greater than 36 *)
let shift log = if List.length log > 36 then List.tl log else log

(** [init_draw_num] is the number of tiles to draw from the bag at the start of
    the game *)
let init_draw_num num_of_players = num_of_players * 7

(** [init_state] creates a state with an empty board, a player with 7 random
    tiles drawn from a bag, and a bag with all tiles except for those 7 *)
let init_state = {
  board = emptyBoard;
  bag = init_bag;
  players = [];
  current_player = {
    name = "undefined";
    dock = [];
    score = 0;
  };
  log = ["Started Game"];
  scoreless = 0
}

(** [add_players state players] creates a new state from the list of player
    names *)
let rec add_players state players =
  match players with
  | x::xs ->
    let new_player = {
      name = x;
      dock = draw_n state.bag 7 |> fst;
      score = 0;
    } in
    let next_state = {
      board = state.board;
      bag = draw_n state.bag 7 |> snd;
      players = new_player::state.players;
      current_player = state.current_player;
      log = state.log;
      scoreless = state.scoreless
    } in
    add_players next_state xs
  | [] ->
    {
      board = state.board;
      bag = state.bag;
      players = state.players;
      current_player = state.players |> List.hd;
      log = state.log;
      scoreless = state.scoreless
    }

(** [tileless_player players] is true if any one of the players has a dock with
    no tiles left in it *)
let rec tileless_player players =
  match players with
  | [] -> false
  | x::xs -> if List.length x.dock = 0 then true else tileless_player xs

(** [game_is_over state] is true when 3 consecutive scoreless turns have passed
    or the bag is empty and a player has no tiles left on their dock *)
let gameover state =
  (state.scoreless = 6) ||
  (List.length state.bag = 0 && tileless_player state.players)

(** [end_message state] is a win message with the name of the player who has
    more points or a tie message if they have they same number of points *)
let end_message state =
  let p1 = (List.nth state.players 0) in
  let p2 = (List.nth state.players 1) in
  if p1.score > p2.score then (p1.name ^ " wins!")
  else if p1.score < p2.score then (p2.name ^ " wins!")
  else "Its a tie"

(** [letter_to_tile] is a function taking a string containing a single letter
    and a dock as input, then returns the tile containing the letter in the dock
    if it exists. *)
let letter_to_tile letter state =
  let rec dock_iter = function
    | [] -> raise BadSelection
    | h::t -> if letter = h.letter then h else (dock_iter t)
  in dock_iter state.current_player.dock

(* alternate using List module function *)
let letter_to_tile letter player =
  try List.find (fun x -> x.letter = letter) player.dock
  with
  | _ -> raise BadSelection

(** [remove_tile_from_dock state tile] returns the player in which [tile] has
    been removed from the current players dock in [state] *)
let remove_tile_from_dock player tile: player =
  let rec dock_iter tile dock acc =
    match dock with
    | [] -> List.rev acc
    | x::xs -> if x = tile then (acc@xs)
      else (dock_iter tile xs (acc@[x])) in
  {
    name = player.name;
    score = player.score;
    dock = dock_iter tile player.dock []
  }

(** [draw_num state] is the number of tiles to draw from the bag, accounting for
    the situation where the bag has less tiles than the player would have to
    draw in order to completely refill their dock *)
let draw_num state =
  let num_played = 7 - List.length state.current_player.dock in
  if num_played <= List.length state.bag then num_played
  else List.length state.bag

(** [pass state] creates a new state where it is the next player's turn and
    nothing else about the state has changed *)
let pass state =
  let to_log = state.current_player.name ^ " passed" in
  {
    board = state.board;
    bag = state.bag;
    players = List.rev (state.current_player::(List.tl (state.players)));
    current_player = List.hd (List.tl state.players);
    log = state.log@[to_log] |> shift;
    scoreless = state.scoreless + 1
  }

(** [play_word state] creates a new state by adding the score from the words
    created during the turn to the player's score, drawing new tiles from the
    bag, and then making it the other players turn *)
let play_word state : (t * string) =
  let num_tiles_played = 7 - List.length state.current_player.dock in
  let (drawn_tiles, new_bag) = draw_n state.bag (draw_num state) in
  let (prescore, words) = calc_score state.board in
  let score = if num_tiles_played = 7 then prescore + 50 else prescore in
  let to_log = state.current_player.name ^ " played " ^
               (String.concat ", " words) ^ " for "  ^
               (string_of_int score) ^ " points" in
  let curr_player = {
    name = state.current_player.name;
    dock = state.current_player.dock @ drawn_tiles;
    score = state.current_player.score + score;
  } in
  ({
    board = finalize state.board;
    bag = new_bag;
    players = List.rev (curr_player::(List.tl (state.players))) ;
    current_player = List.hd (List.tl state.players);
    log = (state.log@[to_log]) |> shift;
    scoreless = 0
  }, string_of_int score)

(** [exchange] t -> string list -> t
    takes in the current game state and a string list from user input and
    removes them from the dock, refills the dock, and adds the tiles taken off
    the dock bag to the bag, effectively "exchanging" the tiles and then makes
    it the other player's turn *)
let exchange state start_letters =
  if (List.length state.current_player.dock <> 7) then raise InvalidExchange
  else if (List.length state.bag < 7) then raise BagTooSmall
  else try begin
    let rec letter_iter player letters tile_acc =
      match letters with
      | x::xs ->
        let tile = letter_to_tile x player in
        letter_iter (remove_tile_from_dock player tile) xs (tile::tile_acc)
      | [] -> (player, tile_acc) in
    let (p, tiles_exchanged) =
      letter_iter state.current_player start_letters [] in
    let (tiles_drawn, new_bag) =
      draw_n state.bag (List.length start_letters) in
    let to_log = state.current_player.name ^ " exchanged " ^
                 (start_letters |> List.length |> string_of_int) ^ " letters" in
    let new_player = {
      name = p.name;
      dock = p.dock @ tiles_drawn;
      score = p.score
    } in
    {
      board = state.board;
      bag = tiles_exchanged@new_bag |> shuffle_bag;
      players = new_player::(List.tl state.players) |> List.rev;
      current_player = List.hd (List.tl state.players);
      log = (state.log@[to_log]) |> shift;
      scoreless = state.scoreless + 1
    } end
    with
    | BadSelection -> raise MissingTilesToExchange

(**[recall st] is the updated [st] after Unfinal tiles are recalled and placed
   back into the current player's dock. *)
let recall st =
  let board_and_pretiles = pop_unfinals st.board in
  {
    board = fst board_and_pretiles;
    bag = st.bag;
    players = st.players;
    current_player = {
      name = st.current_player.name;
      dock = st.current_player.dock@(snd board_and_pretiles);
      score = st.current_player.score;
    };
    log = st.log;
    scoreless = st.scoreless
  }

(** [place_tile state (letter,(row,col))] is the new state after a tile
    corresponding to [letter] has been taken from the current player's dock and
    placed onto the board at the position specified by (row,col). Raises
    BadSelection if there is no tile in the player's dock of that letter. *)
let place_tile state (letter,pos) =
  let tile = letter_to_tile letter state.current_player in
  {
    board = insertTile state.board (Unfinal tile) pos;
    bag = state.bag;
    players = state.players;
    current_player = remove_tile_from_dock state.current_player tile;
    log = state.log;
    scoreless = state.scoreless
  }

(** [pickup_tile state pos] is the new state after the tile at [pos] has been
    placed back into the current player's dock *)
let pickup_tile state pos : (t * string) =
  let (new_board, tile) = (remove_tile state.board pos) in
  ({
    board = new_board;
    bag = state.bag;
    players = state.players;
    log = state.log;
    scoreless = state.scoreless;
    current_player = let p = state.current_player in
      {
        name = p.name;
        score = p.score;
        dock = tile::p.dock;
      };
  }, tile.letter)

(** [get_score state] is the current player's score as a string *)
let get_score state =
  state.current_player.score |> string_of_int

(** [offset tile] is the spaces needed after the value of a tile in order to
    account for differences in number of digits.*)
let offset tile =
  if tile.value >= 10 then "" else " "

(** [print_topline line] prints the top half of [line], where [line] is one row
    of a board *)
let rec print_topline line =
  match line with
  | [] -> print_string [] "|"
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

(** [print_topline line] prints the bottom half of [line], where [line] is one
    row of a board *)
let rec print_botline line =
  match line with
  | [] -> print_string [] "|"
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

(** [print_log log n] prints the nth element of [log] or nothing if that element
    does not exist *)
let print_log log n =
  match List.nth_opt log n with
  | Some msg -> print_string [] ("     " ^ msg)
  | None -> print_string [] ""

(** [print_lineright state n] prints the apropriate information corresponding to
    nth alignment of the board's row dividers *)
let print_lineright state n =
  match n with
  | 3 -> print_log state.log 0
  | 4 -> print_log state.log 3
  | 5 -> print_log state.log 6
  | 6 -> print_log state.log 9
  | 7 -> print_log state.log 12
  | 8 -> print_log state.log 15
  | 9 -> print_log state.log 18
  | 10 -> print_log state.log 21
  | 11 -> print_log state.log 24
  | 12 -> print_log state.log 27
  | 13 -> print_log state.log 30
  | 14 -> print_log state.log 33
  | _ -> print_string [] ""

(** [print_topright state n] prints the apropriate information correspoding to
    the nth alignment of the top section of the tile row *)
let print_topright state n =
  match n with
  | 0 -> let p = (List.nth state.players 0) in
    print_string []
      ("     " ^ p.name ^ "'s score: " ^ (p.score |> string_of_int))
  | 1 -> print_string []
           ("     " ^
            (state.bag |> List.length |> string_of_int) ^ " tiles remaining")
  | 2 -> print_string [] "     Game Log:"
  | 3 -> print_log state.log 1
  | 4 -> print_log state.log 4
  | 5 -> print_log state.log 7
  | 6 -> print_log state.log 10
  | 7 -> print_log state.log 13
  | 8 -> print_log state.log 16
  | 9 -> print_log state.log 19
  | 10 -> print_log state.log 22
  | 11 -> print_log state.log 25
  | 12 -> print_log state.log 28
  | 13 -> print_log state.log 31
  | 14 -> print_log state.log 34
  | _ -> print_string [] ""

(** [print_topright state n] prints the apropriate information correspoding to
    the nth alignment of the bottom section of the tile row *)
let print_botright state n =
  match n with
  | 0 -> let p = (List.nth state.players 1) in
    print_string []
      ("     " ^ p.name ^ "'s score: " ^ (p.score |> string_of_int))
  | 1 -> print_string [] ("     Current Player is " ^ state.current_player.name)
  | 3 -> print_log state.log 2
  | 4 -> print_log state.log 5
  | 5 -> print_log state.log 8
  | 6 -> print_log state.log 11
  | 7 -> print_log state.log 14
  | 8 -> print_log state.log 17
  | 9 -> print_log state.log 20
  | 10 -> print_log state.log 23
  | 11 -> print_log state.log 26
  | 12 -> print_log state.log 29
  | 13 -> print_log state.log 32
  | 14 -> print_log state.log 35
  | _ ->  print_string [] ""

(** [print_board board] prints a graphical representation of [board] into the
    terminal window *)
let print_board board state i =
  let rec print_iter board state i =
    print_string []
      " +————+————+————+————+————+————+————+————+————+————+————+————+————+————+————+";
    print_lineright state i;
    print_endline "";
    match board with
    | [] -> ()
    | x::xs ->
      print_linenum i;
      print_topline x;
      print_topright state i;
      print_endline "";
      print_string [] " ";
      print_botline x;
      print_botright state i;
      print_endline "";
      print_iter xs state (i + 1)
  in
  print_endline
    "   0    1    2    3    4    5    6    7    8    9    10   11   12   13   14";
  print_iter board state i

(** [print_docktop dock] prints the top line of a players dock of tiles *)
let rec print_docktop dock =
  match dock with
  | [] -> print_string [] ""
  | x::xs ->
    print_string tile_style (" " ^ x.letter ^ "  ");
    print_string [] "  "; print_docktop xs

(** [print_dockbot dock] prints the bottom line a players dock of tiles *)
let rec print_dockbot dock =
  match dock with
  | [] -> print_string [] ""
  | x::xs ->
    print_string tile_style ("  " ^ string_of_int x.value ^ offset x);
    print_string [] "  "; print_dockbot xs

(** TODO: docs *)
let dock_offset dock : string =
  let rec extra n acc = if n = 0 then acc else extra (n-1) ("      " ^ acc)
  in "                    " ^ extra (7 - (List.length dock)) ""

(** [print_dock player] prints all of the tiles in a players docks *)
let rec print_dock player msg =
  let dock = player.dock in
  print_string [] "                  "; print_docktop dock;
  print_string [red] ((dock_offset dock) ^ msg ^ "\n");
  print_string [] "                  "; print_dockbot dock;
  print_string [Blink] ((dock_offset dock) ^ "> ")


(** [print_game st] prints the board and dock of the state [st] *)
let print_game st msg =
  print_endline "\n";
  print_board (get_board st) st 0;
  print_endline "";
  print_dock (get_current_player st) msg;
