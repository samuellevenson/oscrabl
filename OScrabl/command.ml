type position = int * int

type command = 
  | Place of string * position
  | Score
  | Quit

exception Blank

exception Broken

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
  let index = (Char.code (Char.uppercase_ascii (String.get str 0))) - 65 in
  if index > 14 then raise Broken else index

(*[valid_col] string -> int
  Takes a numerical string of length one and codes it to an integer 
  representing a board position (0-14). *)
let valid_col str = 
  let index = (Char.code (String.get str 0) - 48) in 
  if index > 14 then raise Broken else index

(*[parse_cmd] string -> command
  Takes a string and returns the appropriate command after parsing.
  Raises:
  Empty if str is empty
    Ex: "" or " "
  Malformed if misspelled verb or incorrect parameters to Command 
    Ex: "score p1" or "place v x2"*)
let rec parse_cmd str = 
  (*Turnes a string into a list separated by spaces*)
  let str_lst = String.split_on_char ' ' str in 
  let command = rm_space str_lst in 
  match command with 
  | [] -> raise Blank
  | h1::h2::h3::h4::[] -> 
    if h1 = "place" then (Place (h2,(single_to_int h3, valid_col h4)))
    else raise Broken
  | h::t -> 
    if h = "score" && t = [] then Score
    else if h = "quit" && t = [] then Quit 
    else raise Broken
