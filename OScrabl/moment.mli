(** Representation of the gamestate at a particular moment *)

exception EmptyBag
exception MissingTilesToExchange
exception InvalidExchange
exception BagTooSmall

type player
type t

(** [create_player] is a function that creates a player object; intended to help
    code reuse. *)
val create_player : string -> Board.pretile list -> int -> player

(** [create_moment] is a function that creates a moment object; intended to help
    code reuse. *)
val create_moment :
  Board.board -> Board.pretile list -> player list -> player -> string list -> t
(** [get_board] is the board in the current game moment.*)
val get_board : t -> Board.board

(** [get_current_player] is the current active player in the game moment. *)
val get_current_player : t -> player

(** [get_name] is the name of the given player. *)
val get_name : player -> string

(** [get_other_player] is the player that is
    not currently active in the game moment. *)
val get_other_player : t -> player

(** [add_players] is the state with the given players added to it. *)
val add_players : t -> string list -> t

(** [get_dock] is the dock of a given player. *)
val get_dock : player -> Board.pretile list

(** [get_current_dock] is the dock of the currently active player. *)
val get_current_dock : t -> Board.pretile list

(** [init_state] is an initial state. *)
val init_state : t

(** [init_bag] is an initial bag. *)
val init_bag : Board.pretile list

(** [shuffle_bag] takes a bag as input and returns it, but shuffled. *)
val shuffle_bag : Board.pretile list -> Board.pretile list

(** [end_message] is a message that displays after the game depending on who,
    if anyone, won the game *)
val end_message : t -> string

(** [game_is_over] is true when an end condition has been reached in the game *)
val gameover : t -> bool

(** [pass] is called when a player passes their turn *)
val pass : t -> t

(** [play_word] is called in order to end the current player's turn.
    Raises:
    InvalidTilePlacement if the tiles are not placed according to the rules of
    scrabble
    InvalidWord if the word(s) created are not in the dictionary *)
val play_word : t -> (t * string)

(** [exchange] is the game state with the current player's choice of tiles
    exchanged.
    Raises:
    InvalidExchange if the player does not have 7 tiles,
    MissingTilesToExchange if the player does not have the
    tiles they want to exchange.
    BagTooSmall if the bag has less than 7 tiles *)
val exchange : t -> string list -> t

(** [recall] is the current game state with all tiles that have been placed in
    the turn recalled. *)
val recall : t -> t

(** [place_tile] is the current game state with a given tile from the user's
    dock placed. *)
val place_tile : t -> (string * (int * int)) -> t

(** [pickup_tile] is tuple game state with a tile removed, and the tile that is
    removed, as a pair. *)
val pickup_tile : t -> (int * int) -> (t * string)

(** [get_score] is the string representation of the score of the
    current player. *)
val get_score : t -> string

(** [print_dock] is the function that prints the dock of the current player. *)
val print_dock : player -> string -> unit

(** [print_game] is the function that prints the game. *)
val print_game : t -> string -> unit

(** [get_player_score] is the score of a given player. *)
val get_player_score : player -> int
