open Yojson.Basic.Util

val file : Yojson.Basic.json

module StringHash : sig 
  type t = string 
  val equal : 'a -> 'a -> bool 
  val hash : 'a -> int 
end

module StringHashTbl : sig 
  type key = StringHash.t
  type 'a t = 'a Hashtbl.Make(StringHash).t
  val create : int -> 'a t
  val clear : 'a t -> unit
  val reset : 'a t -> unit
  val copy : 'a t -> 'a t
  val add : 'a t -> key -> 'a -> unit
  val remove : 'a t -> key -> unit
  val find : 'a t -> key -> 'a
  val find_opt : 'a t -> key -> 'a option
  val find_all : 'a t -> key -> 'a list
  val replace : 'a t -> key -> 'a -> unit
  val mem : 'a t -> key -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val filter_map_inplace : (key -> 'a -> 'a option) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val length : 'a t -> int
  val stats : 'a t -> Hashtbl.statistics
end

(** [word_array] is the array representing the OScrabl dictionary words. *)
val word_array : string array
(** [word_set is the HashTable that will contain the OScrabl dictionary.] *)
val word_set : int StringHashTbl.t
(** [add_hash_set] is the function that actually adds the OScrabl dictionary to 
    the HashTable. *)
val add_hash_set : int StringHashTbl.t -> 
  StringHashTbl.key array -> (StringHashTbl.key -> int) -> unit

(** [validity] is whether a string exists in the OScrabl dictionary. *)
val validity : StringHashTbl.key -> 'a StringHashTbl.t -> bool
(** [delete] removes a key from a HashTable. *)
val delete : StringHashTbl.key -> 'a StringHashTbl.t -> unit
(** [check_words_list] is whether a list of strings is in a HashTable. *)
val check_word_list : string list -> int StringHashTbl.t -> bool