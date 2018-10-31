open Actions
open Moment
open Board
open ANSITerminal



let rec gameplay st msg =
  let _ = Sys.command "clear" in
  print_string [red]
    "             OScrabl by Richard Yu, Samuel Levenson, and Max Chen \n";
  print_game st;
  print_endline msg;
  print_string [red] ("Current player: " ^ st.current_player.name);
  print_string [] "\n> ";
  try
    match parse_cmd (read_line ()) with
    | Place (letter,pos) ->
      gameplay (place_tile st (letter,pos)) ("Placed " ^ letter ^ "!")
    | Pickup pos ->
      let (next_st, tile) = (pickup_tile st pos) in
      gameplay next_st ("Picked up " ^ tile)
    | Score -> gameplay st ("Your score is " ^ get_score st)
    | End ->
      let (next_st, score) = (end_turn st) in
      gameplay next_st ("You scored " ^ score ^ " points. Next turn!")
    | Refill -> gameplay (refill st) "Refilled!"
    | Exchange lst -> gameplay (exchange st lst) "Letters exchanged! Next turn!"
    | Quit -> print_endline "Thanks for playing OScrabl!"; exit 0
    | Recall -> gameplay (recall st) "Tiles recalled!";
    | _ -> exit 0
  with
  | BadSelection -> gameplay st "Bad Tile Selection!";
  | BadRow -> gameplay st "Bad Row input!";
  | BadCol -> gameplay st "Bad Col input!";
  | Broken -> gameplay st "Invalid command!";
  | Blank -> gameplay st "No command given!";
  | Can'tPlaceTile -> gameplay st "Can't place a tile there! Use 'recall' to recall tiles placed on the board!";
  | Can'tPickupTile -> gameplay st "Can't pick up that tile"
  | InvalidWord msg -> gameplay st (msg ^ " is not a word. Use 'recall' to recall tiles placed on the board!")
  | InvalidTilePlacement -> gameplay st "Tiles placed incorrectly! Use 'recall' to recall tiles placed on the board!"
  | InvalidExchange -> gameplay st "You can't exchange with tiles on the board"
  | MissingTilesToExchange -> gameplay st "You don't have the letters you are attempting to exchange!"

let initiate_game () =
  print_string [red] "OScrabl by Richard Yu, Samuel Levenson, and Max Chen \n";
  print_string [red] "Choices: multiplayer or singleplayer. Note: AI currently unimplemented. ";
  print_string [] "\n> ";
  try
    match parse_game_mode (read_line ()) with
    | MultiPlayer ->
      print_string [red] "Enter Player 1's Name.";
      print_string [] "\n> ";
      let p1_name = read_line () in
      print_string [red] "Enter Player 2's Name.";
      print_string [] "\n> ";
      let p2_name = read_line () in
      let p1 = generate_player p1_name init_state in
      let temp_state = generate_initial_state [p1] in

      let p2 = generate_player p2_name temp_state in
      let initial_state = generate_initial_state [p1;p2] in
      gameplay initial_state "Possible Commands: place, recall, quit, exchange, refill."

    | _ -> gameplay init_state "Possible Commands: place, recall, quit, exchange, refill."
  with
  | InvalidGameMode -> gameplay init_state "Can't play selected mode. Starting singleplayer."
  | IncompleteGameMode -> 
    gameplay init_state 
      "SinglePlayer implementation currently incomplete; there is no AI."
(* Temporary placeholder due to lack of singleplayer game mode*)


(** [main ()] unit -> unit
    Prompts for the game to play, then starts it. *)
let main () =
  resize 80 56;
  Words.add_hash_set Words.word_set Words.word_array Hashtbl.hash;
  (* gameplay init_state "Possible Commands: place, recall, quit, exchange, refill." *)
  initiate_game ()

(* Execute the game engine. *)
let () = main ()
