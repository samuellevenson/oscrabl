open Yojson.Basic.Util

(** [file] is the Yojson file containing the valid OScrabl words. *)
let file = Yojson.Basic.from_file "dictionary.json"

module StringHash =
struct
  type t = string
  let equal n1 n2 = (n1=n2)
  let hash i = Hashtbl.hash i
end

module StringHashTbl = Hashtbl.Make(StringHash)

(** [word_array] is the Array of strings inside of
    the OScrabl dictionary json.  *)
let word_array =
  (** [json_list] is the expression representing the list of jsons representing
      words in the OScrabl dictionary. *)
  let json_list = (to_list file) in
  Array.map 
    (fun x -> String.uppercase_ascii (to_string x)) (Array.of_list json_list)

(** [word_set] is the Hash Table that will hold all of the hash values for each
    valid word in the OScrabl dictionary. It is initialized to an empty table, 
    but is mutated using the [add_hash_set] function. *)
let word_set : 
  (int StringHashTbl.t) = StringHashTbl.create (Array.length word_array)

(** [add_hash_set] int StringHashTbl.t -> 
    StringHashTbl.key array -> (StringHashTbl.key -> int) -> unit
    Iterates through an array and hashes a given set*)
let add_hash_set 
    (set: int StringHashTbl.t) arr (hashFn:StringHashTbl.key -> int) =
  Array.iter (fun x -> StringHashTbl.add set x (hashFn x)) arr
(*Hashtbl.add set x (hashFn x)) word_array*)

(** [validity] StringHashTbl.key -> 'a StringHashTbl.t -> bool
    Checks whether a string exists in the OScrabl dictionary. *)
let validity x s = StringHashTbl.mem s x

(** [delete] StringHashTbl.key -> 'a StringHashTbl.t -> unit
    Removes a string from the OScrabl dictionary. *)
let delete x s = StringHashTbl.remove s x

(** [check_word_list] string list -> int StringHashTbl.t -> bool
    is whether an entire list of string exists in a set.
*)
let rec check_word_list (lst:string list) (set:int StringHashTbl.t) = 
  match lst with 
  | [] -> true
  | h::t -> if (validity h set) then check_word_list t set else false
