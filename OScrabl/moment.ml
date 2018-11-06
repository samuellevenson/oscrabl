open Board
open ANSITerminal
open Actions

(** the exception raised when there is an attempt to draw from an empty bag *)
exception EmptyBag
exception MissingTilesToExchange
exception InvalidExchange

(** the type of the player *)
type player = {
  name: string;
  dock: pretile list;
  score: int;
}

(** the type of the moment (state) *)
type t = {
  board: board;
  bag: pretile list;
  players: player list;
  current_player: player;
  log: string list
}

(** [get_board] t -> Board.board
    Returns the board of the current game state. *)
let get_board st = st.board

(** [get_name] player -> string
    Returns the name of the given player. *)
let get_name player = player.name 

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
  log = ["Started Game"]
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
    } in
    add_players next_state xs
  | [] ->
    {
      board = state.board;
      bag = state.bag;
      players = state.players;
      current_player = state.players |> List.hd;
      log = state.log;
    }

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

(** turn ending implemented for 1 player game *)
let play_word state : (t * string) =
  let num_tiles_played = 7 - List.length state.current_player.dock in
  let (drawn_tiles, new_bag) = draw_n state.bag num_tiles_played in
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
    log = (state.log@[to_log]) |> shift
  }, string_of_int score)

(** [exchange] t -> string list -> t
    takes in the current game state and a string list from user input and
    removes them from the dock, then refills the dock,
    effectively "exchanging" the tiles.*)
let exchange state start_letters =
  if (List.length state.current_player.dock <> 7) then raise InvalidExchange
  else try begin
    let rec letter_iter player letters tile_acc =
      match letters with
      | x::xs ->
        let tile = letter_to_tile x player in
        letter_iter (remove_tile_from_dock player tile) xs (tile::tile_acc)
      | [] -> (player, tile_acc) in
    let (p, tiles_exchanged) = letter_iter state.current_player start_letters [] in
    let (tiles_drawn, new_bag) = draw_n state.bag (List.length start_letters) in
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
      log = (state.log@[to_log]) |> shift
    } end
    with
    | BadSelection -> raise MissingTilesToExchange

(**[recall st] is the updated [st] after Unfinal tiles are recalled. *)
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
    log = st.log
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
    log = state.log
  }

(** TODO: docs *)
let pickup_tile state pos : (t * string) =
  let (new_board, tile) = (remove_tile state.board pos) in
  ({
    board = new_board;
    bag = state.bag;
    players = state.players;
    log = state.log;
    current_player =
      let p = state.current_player in
      {
        name = p.name;
        score = p.score;
        dock = tile::p.dock;
      }
  }, tile.letter)

(** TODO: docs *)
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

let print_lineright state i =
  match i with
  | 3 -> begin match (List.nth_opt state.log 0) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 4 -> begin match (List.nth_opt state.log 3) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 5 -> begin match (List.nth_opt state.log 6) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 6 -> begin match (List.nth_opt state.log 9) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 7 -> begin match (List.nth_opt state.log 12) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 8 -> begin match (List.nth_opt state.log 15) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 9 -> begin match (List.nth_opt state.log 18) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 10 -> begin match (List.nth_opt state.log 21) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 11 -> begin match (List.nth_opt state.log 24) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 12 -> begin match (List.nth_opt state.log 27) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 13 -> begin match (List.nth_opt state.log 30) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 14 -> begin match (List.nth_opt state.log 33) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | _ -> print_string [] ""

(** print the score *)
let print_topright state i =
  match i with
  | 0 -> let p = (List.nth state.players 0) in
    print_string [] ("     " ^ p.name ^ "'s score: " ^ (p.score |> string_of_int))
  | 1 -> print_string [] ("     " ^ (state.bag |> List.length |> string_of_int) ^ " tiles remaining")
  | 2 -> print_string [] "     Game Log:"
  | 3 -> begin match (List.nth_opt state.log 1) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 4 -> begin match (List.nth_opt state.log 4) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 5 -> begin match (List.nth_opt state.log 7) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 6 -> begin match (List.nth_opt state.log 10) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 7 -> begin match (List.nth_opt state.log 13) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 8 -> begin match (List.nth_opt state.log 16) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 9 -> begin match (List.nth_opt state.log 19) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 10 -> begin match (List.nth_opt state.log 22) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 11 -> begin match (List.nth_opt state.log 25) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 12 -> begin match (List.nth_opt state.log 28) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 13 -> begin match (List.nth_opt state.log 31) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 14 -> begin match (List.nth_opt state.log 34) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | _ -> print_string [] ""

let print_botright state i =
  match i with
  | 0 -> let p = (List.nth state.players 1) in
    print_string [] ("     " ^ p.name ^ "'s score: " ^ (p.score |> string_of_int))
  | 1 -> print_string [] ("     Current Player is " ^ state.current_player.name)
  | 3 -> begin match (List.nth_opt state.log 2) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 4 -> begin match (List.nth_opt state.log 5) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 5 -> begin match (List.nth_opt state.log 8) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 6 -> begin match (List.nth_opt state.log 11) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 7 -> begin match (List.nth_opt state.log 14) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 8 -> begin match (List.nth_opt state.log 17) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 9 -> begin match (List.nth_opt state.log 20) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 10 -> begin match (List.nth_opt state.log 23) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 11 -> begin match (List.nth_opt state.log 26) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 12 -> begin match (List.nth_opt state.log 29) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 13 -> begin match (List.nth_opt state.log 32) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | 14 -> begin match (List.nth_opt state.log 35) with
      | Some msg -> print_string [] ("     " ^ msg)
      | None -> print_string [] ""
    end
  | _ ->  print_string [] ""

(** [print_board board] prints a graphical representation of [board] into the
    terminal window *)
let print_board board state i =
  let rec print_iter board state i =
    print_string [] " +————+————+————+————+————+————+————+————+————+————+————+————+————+————+————+";
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
  print_endline "   0    1    2    3    4    5    6    7    8    9    10   11   12   13   14";
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

let dock_offset dock : string =
  let rec extra n acc = if n = 0 then acc else extra (n-1) ("      " ^ acc)
  in "                    " ^ extra (7 - (List.length dock)) ""

(** [print_dock player] prints all of the tiles in a players docks *)
let rec print_dock player msg =
  let dock = player.dock in
  print_string [] "                  "; print_docktop dock;
  print_string [red] ((dock_offset dock) ^ msg ^ "\n");
  print_string [] "                  "; print_dockbot dock;
  print_string [] ((dock_offset dock) ^ "> ")


(** [print_game st] prints the board and dock of the state [st] *)
let print_game st msg =
  print_endline "\n";
  print_board (get_board st) st 0;
  print_endline "";
  print_dock (get_current_player st) msg;
