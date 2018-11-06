type position = int * int

type action =
  | Place of string * position
  | Pickup of position
  | Score
  | End
  | Draw
  | Help
  | Exchange of string list
  | Refill
  | Recall
  | Quit

type game_mode =
  | SinglePlayer
  | MultiPlayer
  | QuitGame

exception InvalidGameMode

exception Blank

exception Broken

exception BadRow

exception BadCol

exception BadSelection

(** [rm_space] string list -> string list
    A function that removes all empty strings in [lst] *)
let rec rm_space lst =
  match lst with
  | [] -> []
  | h::t ->
    if h = "" then rm_space t
    else h::rm_space t

(** [single_to_int] string -> int
    Takes a string of length one and codes it to an integer representing a board
    position (0-14). *)
let single_to_int str =
  let rowindex = (Char.code (Char.uppercase_ascii (String.get str 0))) - 65 in
  if rowindex > 14 then raise BadRow else rowindex

(** [check_int] string -> boolean
    returns true if a string is an int. *)
let check_int s = 
  try int_of_string s |> ignore; true
  with Failure _ -> false

(** [to_upper_case] string list -> string list
    is a character string list with every string capitalized. 
    If a string contains an integer, BadSelectino is raised. *)
let to_upper_case lst =
  List.map (fun x -> if check_int x then raise BadSelection else String.uppercase_ascii x) lst

(** [parse_game_mode] string -> game_mode
    is the user's input parsed into a game mode.
    Raises: 
    InvalidGameMode if something other than multiplayer, singleplayer,
    or quit is given. *)
let rec parse_game_mode str =
  let str_lst = String.split_on_char ' ' str in
  let gm = rm_space str_lst in
  match gm with
  | [] -> raise Blank
  | h::t ->
    if h = "multiplayer" then MultiPlayer
    else if h = "quit" then QuitGame
    else if h = "singleplayer" then SinglePlayer
    else raise InvalidGameMode


(** [parse_cmd] str -> action
    is the user's input parsed into an action.
    Raises: 
    Blank of user input is empty, 
    Broken if an invalid pattern for an action is given. *)
let rec parse_cmd str =
  (*Turnes a string into a list separated by spaces*)
  let str_lst = String.split_on_char ' ' str in
  let action = rm_space str_lst in
  match action with
  | [] -> raise Blank
  | h1::h2::h3::h4::[] when h1 = "place" ->
    Place ((String.uppercase_ascii h2),(single_to_int h3, int_of_string h4))
  | h1::h2::h3::[] when h1 = "pickup" ->
    Pickup (single_to_int h2, int_of_string h3)
  | h::t ->
    if h = "score" && t = [] then Score
    else if h = "refill" && t = [] then Refill
    else if h = "help" && t = [] then Help
    else if h = "exchange" && (List.length t > 0) then Exchange (to_upper_case t)
    else if h = "end" && t = [] then End
    else if h = "draw" && t = [] then Draw
    else if h = "quit" && t = [] then Quit
    else if h = "recall" && t = [] then Recall
    else raise Broken
