open Board
open Actions

(*let start_game =
  print_board emptyBoard*)

let rec gameplay board iteration =
  if iteration = 0 then (
    (ANSITerminal.resize 110 39);
    gameplay board (iteration + 1))
  else
    let not_used = Sys.command"clear" in
    (ANSITerminal.(print_string [red] 
                     "\n\n             OScrable by Richard Yu, Samuel Levenson, and Max Chen \n"));
    print_board board 0;
    print_newline();
    print_string "Possible Commands:";
    print_newline();
    print_string "place, quit";
    print_newline(); 
    print_string  "> ";
    (match (parse_cmd (read_line ())) with
     | Quit -> 
       print_string "Thanks for playing OScrabl!";
       print_newline(); 
       exit 0
     | Place (tile,(row,col)) -> gameplay (insertTile board (Some {letter=tile; value= 5}) (row,col)) (iteration + 1)
     | _ -> exit 0
    )



(** [main ()] unit -> unit 
    Prompts for the game to play, then starts it. *)
let main () =
  (*ANSITerminal.(print_string [red]
                  "\n\nWelcome to OScrabl.\n");
    print_endline "Please enter the name of the game file you want to load.\n";
    print_string  "> ";*)
  gameplay emptyBoard 0
(*match read_line () with
  | exception End_of_file -> ()
  | file_name -> play_game*)


(* Execute the game engine. *)
let () = main ()