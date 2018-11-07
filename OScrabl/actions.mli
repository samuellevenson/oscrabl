(** Parse player (and AI) input *)

(** position is a type representing a coordinate. *)
type position = int * int
(** action is a type representing an action given by the user. *)
type action =
  | Place of string * position
  | Pickup of position
  | Score
  | End
  | Pass
  | Help
  | Exchange of string list
  | Refill
  | Recall
  | Quit



(** game_mode is a type representing the game mode selected by the
    user. *)
type game_mode =
  | SinglePlayer
  | MultiPlayer
  | Spectator
  | QuitGame

exception InvalidGameMode

exception Blank

exception Broken

exception BadRow

exception BadCol

exception BadSelection

(** [check_int] is whether or not a string has an integer in it. *)
val check_int : string -> bool

(** [to_upper_case] is the given string list with every string capitalized.
    Raises:
    BadSelection if the given string contains an integer. *)
val to_upper_case : string list -> string list

(** [parse_game_mode] is the selected game mode from the given user input.
    Raises:
    InvalidGameMode if something other than multiplayer, singleplayer,
    or quit is given. *)
val parse_game_mode : string -> game_mode

(** [parse_cmd] is the selected action given by user input.
    Raises:
    Blank of user input is empty,
    Broken if an invalid pattern for an action is given. *)
val parse_cmd : string -> action

(** [single_to_int]
    Takes a string of length one and codes it to an integer representing a board
    position (0-14). *)
val single_to_int : string -> int
