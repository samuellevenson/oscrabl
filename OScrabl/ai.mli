(** The scrabble playing computer *)

open Actions
open Moment
open Words
open Board

exception InvalidSize
exception Test

(** the type for ai a*)
type brain

(** [ai_actions] is the actions that the AI will perform. *)
val ai_actions : Moment.t -> Actions.action list
