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
  print_string [] "\n> ";
  try
    match parse_cmd (read_line ()) with
    | Place (letter,pos) ->
      gameplay (place_tile st (letter,pos)) ("Placed " ^ letter ^ "!")
    | Score -> gameplay st (get_score st)
    | End -> gameplay (end_turn st) "Next turn!"
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
  | InvalidWord msg -> gameplay st (msg ^ " is not a word. Use 'recall' to recall tiles placed on the board!")
  | InvalidTilePlacement -> gameplay st "Tiles placed incorrectly! Use 'recall' to recall tiles placed on the board!"
  | InvalidExchange -> gameplay st
                         "You don't have the letters you are attempting to exchange!"

(** [main ()] unit -> unit
    Prompts for the game to play, then starts it. *)
let main () =
  resize 80 56;
  Words.add_hash_set Words.word_set Words.word_array Hashtbl.hash;
  gameplay init_state "Possible Commands: place, recall, quit, exchange, refill."

(* Execute the game engine. *)
let () = main ()
