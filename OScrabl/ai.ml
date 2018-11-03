open Actions 
open Moment
open Words
open Board

exception InvalidSize

(** the type for ai a*)
type brain = {original_state: Moment.t; 
              hypothetical_state: Moment.t; 
              actions: Actions.action list}

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

(** [minimize7 cL] is [cL] without any of its elements after index 6, if there
    are any. *)
let minimize7 coordList = 
  if List.length coordList > 7 then 
    [List.nth coordList 0; List.nth coordList 1;
     List.nth coordList 2; List.nth coordList 3; List.nth coordList 4;
     List.nth coordList 5; List.nth coordList 6] else 
    coordList

(** [minimize6 cL] is [cL] without any of its elements after index 5, if there
    are any. *)
let minimize6 coordList = 
  if List.length coordList > 6 then 
    [List.nth coordList 0; List.nth coordList 1;
     List.nth coordList 2; List.nth coordList 3; List.nth coordList 4;
     List.nth coordList 5;] else 
    coordList

(** [minimize5 cL] is [cL] without any of its elements after index 4, if there
    are any. *)
let minimize5 coordList = 
  if List.length coordList > 5 then 
    [List.nth coordList 0; List.nth coordList 1;
     List.nth coordList 2; List.nth coordList 3; List.nth coordList 4;] else 
    coordList

(** [minimize4 cL] is [cL] without any of its elements after index 3, if there
    are any. *)
let minimize4 coordList = 
  if List.length coordList > 4 then 
    [List.nth coordList 0; List.nth coordList 1;
     List.nth coordList 2; List.nth coordList 3;] else 
    coordList

(** [minimize3 cL] is [cL] without any of its elements after index 2, if there
    are any. *)
let minimize3 coordList = 
  if List.length coordList > 3 then 
    [List.nth coordList 0; List.nth coordList 1;
     List.nth coordList 2;] else 
    coordList
(** [minimize2 cL] is [cL] without any of its elements after index 1, if there
    are any. *)
let minimize2 coordList = 
  if List.length coordList > 2 then 
    [List.nth coordList 0; List.nth coordList 1;] else 
    coordList

(** [minimize1 cL] is [cL] without any of its elements after index 0, if there
    are any. *)
let minimize1 coordList = 
  if List.length coordList > 1 then 
    [List.nth coordList 0] else 
    coordList




(** [vertical_tile_placements b c s] is the list of coordinates for vertical tile
    placements anchored on coordinate [c] on [b] that one can make. The length 
    of the list is limited to [2 * size]. Raises InvalidSize if [size] > 7 or 
    [size] < 1 Arranged from bottom to top. Example: [(7,8), (7,6), (7,5)], 
    anchored on a board with only a single Final tile placed on (7,7). *)
let vertical_tile_placements b (x,y) size: (int * int) list = 
  if (size > 7) then raise InvalidSize else 
  if (size = 7) then 
    let upward_sqrs = minimize7 (emptySqrs_up b (x,y)) in 
    let downward_sqrs = minimize7 (emptySqrs_down b (x,y)) in 
    List.rev downward_sqrs@upward_sqrs else 
  if (size = 6) then 
    let upward_sqrs = minimize6 (emptySqrs_up b (x,y)) in 
    let downward_sqrs = minimize6 (emptySqrs_down b (x,y)) in 
    List.rev downward_sqrs@upward_sqrs else 
  if (size = 5) then 
    let upward_sqrs = minimize5 (emptySqrs_up b (x,y)) in 
    let downward_sqrs = minimize5 (emptySqrs_down b (x,y)) in 
    List.rev downward_sqrs@upward_sqrs else 
  if (size = 4) then 
    let upward_sqrs = minimize4 (emptySqrs_up b (x,y)) in 
    let downward_sqrs = minimize4 (emptySqrs_down b (x,y)) in 
    List.rev downward_sqrs@upward_sqrs else 
  if (size = 3) then 
    let upward_sqrs = minimize3 (emptySqrs_up b (x,y)) in 
    let downward_sqrs = minimize3 (emptySqrs_down b (x,y)) in 
    List.rev downward_sqrs@upward_sqrs else 
  if (size = 2) then 
    let upward_sqrs = minimize2 (emptySqrs_up b (x,y)) in 
    let downward_sqrs = minimize2 (emptySqrs_down b (x,y)) in 
    List.rev downward_sqrs@upward_sqrs else 
  if (size = 1) then 
    let upward_sqrs = minimize1 (emptySqrs_up b (x,y)) in 
    let downward_sqrs = minimize1 (emptySqrs_down b (x,y)) in 
    List.rev downward_sqrs@upward_sqrs else raise InvalidSize

(** [horizontal_tile_placements b c s] is the list of coordinates for horizontal
    tile placements anchored on coordinate [c] on [b] that one can make. The length 
    of the list is limited to [2 * size]. Raises InvalidSize if [size] > 7 or 
    [size] < 1. Arranged from left to right. Example: [(6,7), (8,7), (9,7)], 
    anchored on a board with only a single Final tile placed on (7,7).*)
let horizontal_tile_placements b (x,y) size: (int * int) list = 
  if (size > 7) then raise InvalidSize else 
  if (size = 7) then 
    let leftward_sqrs = minimize7 (emptySqrs_left b (x,y)) in 
    let rightward_sqrs = minimize7 (emptySqrs_right b (x,y)) in 
    List.rev leftward_sqrs@rightward_sqrs else 
  if (size = 6) then 
    let leftward_sqrs = minimize6 (emptySqrs_left b (x,y)) in 
    let rightward_sqrs = minimize6 (emptySqrs_right b (x,y)) in 
    List.rev leftward_sqrs@rightward_sqrs else 
  if (size = 5) then 
    let leftward_sqrs = minimize5 (emptySqrs_left b (x,y)) in 
    let rightward_sqrs = minimize5 (emptySqrs_right b (x,y)) in 
    List.rev leftward_sqrs@rightward_sqrs else 
  if (size = 4) then 
    let leftward_sqrs = minimize4 (emptySqrs_left b (x,y)) in 
    let rightward_sqrs = minimize4 (emptySqrs_right b (x,y)) in 
    List.rev leftward_sqrs@rightward_sqrs else 
  if (size = 3) then 
    let leftward_sqrs = minimize3 (emptySqrs_left b (x,y)) in 
    let rightward_sqrs = minimize3 (emptySqrs_right b (x,y)) in 
    List.rev leftward_sqrs@rightward_sqrs else 
  if (size = 2) then 
    let leftward_sqrs = minimize2 (emptySqrs_left b (x,y)) in 
    let rightward_sqrs = minimize2 (emptySqrs_right b (x,y)) in 
    List.rev leftward_sqrs@rightward_sqrs else 
  if (size = 1) then 
    let leftward_sqrs = minimize1 (emptySqrs_left b (x,y)) in 
    let rightward_sqrs = minimize1 (emptySqrs_right b (x,y)) in 
    List.rev leftward_sqrs@rightward_sqrs else raise InvalidSize

let ai's_moves (cur_st:Moment.t): Actions.action list = 
  let cur_brain = {original_state = cur_st; 
                   hypothetical_state = cur_st; 
                   actions = []} in 

  failwith"un"
