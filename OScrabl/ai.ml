open Actions 
open Moment
open Words
open Board

(** [emptySqrs_down b c] is the list of coordinates of connected [Nothing] tile 
    squares that are vertically below [c] on [b].*)
let emptySqrs_down (b: board) ((x,y): (int * int)): (int * int) list = 
  let rec helper board (x,y) accList= 
    if ((y+1) = 15) then (List.rev accList) else 
      begin match Board.get_square board (x,y+1)  with 
        | Nothing, _ -> helper board (x, y+1) ((x,y+1)::accList)
        | _ -> List.rev accList 
      end in helper b (x,y) []

(** [emptySqrs_up b c] is the list of coordinates of connected [Nothing] tile 
    squares that are vertically above [c] on [b].*)
let emptySqrs_up (b: board) ((x,y): (int * int)): (int * int) list =
  let rec helper board (x,y) accList= 
    if ((y-1) = -1) then (List.rev accList) else 
      begin match Board.get_square board (x,y-1)  with 
        | Nothing, _ -> helper board (x, y-1) ((x,y-1)::accList)
        | _ -> List.rev accList 
      end in helper b (x,y) []

(** [emptySqrs_left b c] is the list of coordinates of connected [Nothing] tile 
    squares that are horizontally left from [c] on [b].*)
let emptySqrs_left (b: board) ((x,y): (int * int)): (int * int) list =
  let rec helper board (x,y) accList= 
    if ((x-1) = -1) then (List.rev accList) else 
      begin match Board.get_square board (x-1,y)  with 
        | Nothing, _ -> helper board (x-1, y) ((x-1,y)::accList)
        | _ -> List.rev accList 
      end in helper b (x,y) []

(** [emptySqrs_right b c] is the list of coordinates of connected [Nothing] tile 
    squares that are horizontally right from [c] on [b].*)
let emptySqrs_right (b: board) ((x,y): (int * int)): (int * int) list =
  let rec helper board (x,y) accList= 
    if ((x+1) = 15) then (List.rev accList) else 
      begin match Board.get_square board (x+1,y)  with 
        | Nothing, _ -> helper board (x+1, y) ((x+1,y)::accList)
        | _ -> List.rev accList 
      end in helper b (x,y) []
