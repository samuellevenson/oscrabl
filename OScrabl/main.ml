open Actions
open Moment
open Board
open ANSITerminal

(** TODO: docs *)
let ai_perform_actions initial_state : (Moment.t * string) =
  print_game initial_state "The computer is thinking";
  let rec repeat state action_list msg =
    match action_list with
    | x::xs -> begin match x with
        | Place (letter,pos) -> repeat (place_tile state (letter,pos)) xs ""
        | End ->
          let (next_st, score) = (play_word state) in
          repeat next_st xs ("The computer scored " ^ score ^ " points")
        | Exchange lst -> 
          repeat (exchange state lst) xs "The computer exchanged its tiles"
        | _ -> failwith "ai should not issue any other command"
      end
    | [] -> (state, msg)
  in repeat initial_state (Ai.ai_actions initial_state) ""

(** TODO: docs *)
let rec gameplay st msg =
  let _ = Sys.command "clear" in
  if (get_name (get_current_player st)) = "Computer" then
    let (next_state, msg) =  ai_perform_actions st in gameplay next_state msg
  else begin
    print_game st msg;
    try
      match parse_cmd (read_line ()) with
      | Place (letter,pos) ->
        gameplay (place_tile st (letter,pos)) ("Placed " ^ letter ^ "!")
      | Pickup pos ->
        let (next_st, tile) = (pickup_tile st pos) in
        gameplay next_st ("Picked up " ^ tile)
      | Score -> gameplay st ("Your score is " ^ get_score st)
      | Help -> gameplay st
                  ("Your available actions are: place, score,
                 recall, quit, exchange, pickup, help.")
      | End ->
        let (next_st, score) = (play_word st) in
        gameplay next_st ("You scored " ^ score ^ " points. Next turn!")
      | Exchange lst -> 
        gameplay (exchange st lst) "Letters exchanged! Next turn!"
      | Quit -> print_endline "Thanks for playing OScrabl!"; exit 0
      | Recall -> gameplay (recall st) "Tiles recalled!";
      | _ -> exit 0
    with
    | BadSelection -> gameplay st "Bad Tile Selection!";
    | BadRow -> gameplay st "Bad Row input!";
    | BadCol -> gameplay st "Bad Col input!";
    | Broken -> gameplay st "Invalid command!";
    | Blank -> gameplay st "No command given!";
    | Can'tPlaceTile -> gameplay st "Can't place a tile there";
    | Can'tPickupTile -> gameplay st "Can't pick up that tile"
    | InvalidWord msg -> gameplay st (msg ^ " is not a word")
    | InvalidTilePlacement -> gameplay st "Tiles placed incorrectly"
    | InvalidExchange -> 
      gameplay st "You can't exchange with tiles on the board"
    | MissingTilesToExchange -> gameplay st "You don't have those letters"
  end

(** TODO: docs *)
let rec initiate_game () =
  let _ = Sys.command "clear" in
  print_string [magenta] "














                                     ██████╗ ███████╗ ██████╗██████╗  █████╗ ██████╗ ██╗
                                    ██╔═══██╗██╔════╝██╔════╝██╔══██╗██╔══██╗██╔══██╗██║
                                    ██║   ██║███████╗██║     ██████╔╝███████║██████╔╝██║
                                    ██║   ██║╚════██║██║     ██╔══██╗██╔══██║██╔══██╗██║
                                    ╚██████╔╝███████║╚██████╗██║  ██║██║  ██║██████╔╝███████╗
                                     ╚═════╝ ╚══════╝ ╚═════╝╚═╝  ╚═╝╚═╝  ╚═╝╚═════╝ ╚══════╝



";
  print_string [Bold] "                                        ";
  print_string [Bold] "by Richard Yu, Samuel Levenson, and Max Chen\n";
  print_string [] "\n\n\n\n\n\n\n\n\n";
  print_string [] "                                           ";
  print_string [] "Choices: multiplayer or singleplayer\n";
  print_string [Blink] "                                           > ";
  try
    match parse_game_mode (read_line ()) with
    | MultiPlayer ->
      print_string [red] "Enter Player 1's Name.";
      print_string [] "\n> ";
      let p1 = read_line () in
      print_string [red] "Enter Player 2's Name.";
      print_string [] "\n> ";
      let p2 = read_line () in
      gameplay (add_players init_state [p2;p1]) ("Started multiplayer game")
    | SinglePlayer -> 
      gameplay 
        (add_players init_state 
           ["Computer";"Player"]) "Started singleplayer game"
    | QuitGame -> print_endline "Thanks for playing OScrabl!"; exit 0
  with
  | InvalidGameMode -> print_endline "???"; initiate_game ()

(** [main ()] unit -> unit
    Prompts for the game to play, then starts it. *)
let main () =
  resize 125 50;
  Words.add_hash_set Words.word_set Words.word_array Hashtbl.hash;
  initiate_game ()

(* Execute the game engine. *)
let () = main ()
