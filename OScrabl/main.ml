open Actions
open Moment
open Board

(*let start_game =
  print_board emptyBoard*)

let rec tile_to_str_list (lst: tile list) acc = 
  match lst with 
  | [] -> acc
  | h::t -> tile_to_str_list t (h.letter::acc)

let rec check_holding_tile attempted_tile lst = 
  match lst with 
  | [] -> false
  | h::t -> if h = attempted_tile then true 
    else (check_holding_tile attempted_tile t)

(* let rec remove_added_tile added_tile lst acc = 
   match lst with 
   | [] -> acc
   | h::t -> if h = added_tile then  *)


let rec gameplay st msg =
  let not_used = Sys.command"clear" in
  (ANSITerminal.(print_string [red] 
                   "\n\n             OScrable by Richard Yu, Samuel Levenson, and Max Chen \n"));
  print_game st; print_newline(); print_string "Possible Commands:";
  print_newline(); print_string "place, quit"; print_newline();
  print_newline(); print_string msg; print_newline();
  print_string  "> ";
  (try 
     let cmd = parse_cmd (read_line ()) in 
     match cmd with
     | Place (tile,(row,col)) -> 

       (let tile_str_list = (tile_to_str_list st.current_player.dock []) in 
        if (check_holding_tile tile tile_str_list) then  
          let updated_st = (update_state st cmd) in gameplay updated_st msg
        else raise BadSelection)
     | Quit -> print_string "Thanks for playing OScrabl!"; print_newline(); exit 0
     | _ -> exit 0
   with 

   | BadSelection -> gameplay st "Bad Tile Selection.";
   | BadRow -> gameplay st "Bad Row input.";
   | BadCol -> gameplay st "Bad Col input.";
   | Broken -> gameplay st "Invalid action.";
   | Blank -> gameplay st "No action given.";
   | _ -> gameplay st "Exception encountered.")





(** [main ()] unit -> unit 
    Prompts for the game to play, then starts it. *)
let main () =
  (ANSITerminal.resize 110 51);
  let start_state = init_state in 
  gameplay start_state ""

(* Execute the game engine. *)
let () = main ()