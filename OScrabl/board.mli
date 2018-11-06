(** Functions and types for the board *)

open ANSITerminal
open Words

exception Can'tPlaceTile
exception Can'tPickupTile
exception InvalidSquare
exception InvalidRow
exception InvalidColumn
exception NothingSquare
exception InvalidWord of string
exception InvalidTilePlacement

(** The type of tiles *)
type pretile = {
  letter: string; value: int
}

type tile = Final of pretile | Unfinal of pretile | Nothing

(** The type of score multipliers *)
type multiplier = DoubleLetter | TripleLetter | DoubleWord | TripleWord | NaN

(** The type of squares *)
type square = tile * multiplier

(** The type of the scrabble® board *)
type board = square list list

(** [tile_style] is the ANSITerminal style list for letter tiles *)
val tile_style : ANSITerminal.style list

(** [emptyBoard] is an empty game board used at the start of the game. *)
val emptyBoard: board

(** [is_firstmove] is whether it is currently the first move. *)
val is_firstmove : board -> bool

(** [get_square] is the square at a given coordinate. *)
val get_square : 'a list list -> int * int -> 'a

(** [insertTile] is the OScrabl board with a tile inserted into it. *)
val insertTile: (tile * 'a) list list ->
  tile -> int * int -> (tile * 'a) list list

(* [pop_unfinals] is a (board, unfinal tile list) tuple, where
   the board has all unfinal tiles removed.*)
val pop_unfinals : square list list -> board * pretile list

(** [remove_tile] is the OScrabl board with a tile removed from it. *)
val remove_tile: (tile * 'a) list list -> int * int ->
  (tile * 'a) list list * pretile

(** [calc_score] is the score of the current player. *)
val calc_score : board -> (int * string list)

(** [finalize_board board] turns all the Unfinal tiles into Final tiles *)
val finalize : square list list -> board
