open Actions
open Moment

(*let start_game =
  print_board emptyBoard*)

let rec gameplay st =
  let not_used = Sys.command"clear" in
  (ANSITerminal.(print_string [red] 
                   "\n\n             OScrable by Richard Yu, Samuel Levenson,
                    and Max Chen \n"));
  print_game st; print_newline(); print_string "Possible Commands:";
  print_newline(); print_string "place, quit"; print_newline();
  print_string  "> ";

  let cmd = parse_cmd (read_line ()) in 
  match cmd with
  | Place (tile,(row,col)) -> 
    let updated_st = (update_st st cmd) in gameplay updated_st
  | Exit -> print_string "Thanks for playing OScrabl!"; print_newline(); exit 0
  | _ -> exit 0




(** [main ()] unit -> unit 
    Prompts for the game to play, then starts it. *)
let main () =
  (ANSITerminal.resize 110 51);
  let start_state = init_state in 
  gameplay start_state 

(* Execute the game engine. *)
let () = main ()