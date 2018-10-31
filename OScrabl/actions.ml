type position = int * int

type action =
  | Place of string * position
  | Pickup of position
  | Score
  | End
  | Draw
  | Exchange of string list
  | Refill
  | Recall
  | Quit

type game_mode =
  | SinglePlayer
  | MultiPlayer

exception IncompleteGameMode

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

(*[single_to_int] string -> int
  Takes a string of length one and codes it to an integer representing a board
  position (0-14). *)
let single_to_int str =
  let rowindex = (Char.code (Char.uppercase_ascii (String.get str 0))) - 65 in
  if rowindex > 14 then raise BadRow else rowindex

(*[valid_col] string -> int
  Takes a numerical string of length one and codes it to an integer
  representing a board position (0-14). *)
(*let valid_col str =
  if String.length str > 1 then
    let colindex = (Char.code (String.get str 0) - 48) + (Char.code (String.get str 1) - 48) in
    if colindex > 14 then raise BadCol else colindex
  else
    let colindex = (Char.code (String.get str 0) - 48) in
    if colindex > 14 then raise BadCol else colindex*)

(*[parse_cmd] string -> command
  Takes a string and returns the appropriate command after parsing.
  Raises:
  Empty if str is empty
    Ex: "" or " "
  Malformed if misspelled verb or incorrect parameters to Command
    Ex: "score p1" or "place v x2"*)

let to_upper_case lst =
  List.map (fun x -> String.uppercase_ascii x) lst

let rec parse_game_mode str =
  let str_lst = String.split_on_char ' ' str in
  let gm = rm_space str_lst in
  match gm with
  | [] -> raise Blank
  | h::t -> if h = "multiplayer" then MultiPlayer
    else if h = "singleplayer" then raise IncompleteGameMode
    else raise InvalidGameMode

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
    else if h = "exchange" && (List.length t > 0) then Exchange (to_upper_case t)
    else if h = "end" && t = [] then End
    else if h = "draw" && t = [] then Draw
    else if h = "quit" && t = [] then Quit
    else if h = "recall" && t = [] then Recall
    else raise Broken
