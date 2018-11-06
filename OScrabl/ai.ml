open Actions
open Moment
open Words
open Board

exception InvalidSize
exception Test

(** the type for ai a*)
type brain = {original_state: Moment.t;
              hypothetical_state: Moment.t;
              actions: Actions.action list}

(** [emptySqrs_down b c] is the list of coordinates of connected [Nothing] tile
    squares that are vertically below [c] on [b].*)
let emptySqrs_right (b: board) ((x,y): (int * int)): (int * int) list =
  let rec helper board (x,y) accList=
    if ((y+1) = 15) then (List.rev accList) else
      begin match Board.get_square board (x,y+1)  with
        | Nothing, _ -> helper board (x, y+1) ((x,y+1)::accList)
        | _ -> List.rev accList
      end in helper b (x,y) []

(** [emptySqrs_up b c] is the list of coordinates of connected [Nothing] tile
    squares that are vertically above [c] on [b].*)
let emptySqrs_left (b: board) ((x,y): (int * int)): (int * int) list =
  let rec helper board (x,y) accList=
    if ((y-1) = -1) then (List.rev accList) else
      begin match Board.get_square board (x,y-1)  with
        | Nothing, _ -> helper board (x, y-1) ((x,y-1)::accList)
        | _ -> List.rev accList
      end in helper b (x,y) []

(** [emptySqrs_left b c] is the list of coordinates of connected [Nothing] tile
    squares that are horizontally left from [c] on [b].*)
let emptySqrs_up (b: board) ((x,y): (int * int)): (int * int) list =
  let rec helper board (x,y) accList=
    if ((x-1) = -1) then (List.rev accList) else
      begin match Board.get_square board (x-1,y)  with
        | Nothing, _ -> helper board (x-1, y) ((x-1,y)::accList)
        | _ -> List.rev accList
      end in helper b (x,y) []

(** [emptySqrs_right b c] is the list of coordinates of connected [Nothing] tile
    squares that are horizontally right from [c] on [b].*)
let emptySqrs_down (b: board) ((x,y): (int * int)): (int * int) list =
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




(** [vertical_tile_placements b c s] is the list of coordinates for possible
    vertical tile placements anchored on coordinate [c] on [b] that one can
    make. The length of the list is limited to [2 * size]. Raises InvalidSize
    if [size] > 7 or [size] < 1. Arranged from bottom to top.
    Example: [(7,8), (7,6), (7,5)], anchored on a board with only a single Final
    tile placed on (7,7). *)
let vertical_tile_placements b (y,x) size: (int * int) list =
  if (size > 7) then raise InvalidSize else
  if (size = 7) then
    let upward_sqrs = minimize7 (emptySqrs_up b (y,x)) in
    let downward_sqrs = minimize7 (emptySqrs_down b (y,x)) in
    List.rev downward_sqrs@upward_sqrs else
  if (size = 6) then
    let upward_sqrs = minimize6 (emptySqrs_up b (y,x)) in
    let downward_sqrs = minimize6 (emptySqrs_down b (y,x)) in
    List.rev downward_sqrs@upward_sqrs else
  if (size = 5) then
    let upward_sqrs = minimize5 (emptySqrs_up b (y,x)) in
    let downward_sqrs = minimize5 (emptySqrs_down b (y,x)) in
    List.rev downward_sqrs@upward_sqrs else
  if (size = 4) then
    let upward_sqrs = minimize4 (emptySqrs_up b (y,x)) in
    let downward_sqrs = minimize4 (emptySqrs_down b (y,x)) in
    List.rev downward_sqrs@upward_sqrs else
  if (size = 3) then
    let upward_sqrs = minimize3 (emptySqrs_up b (y,x)) in
    let downward_sqrs = minimize3 (emptySqrs_down b (y,x)) in
    List.rev downward_sqrs@upward_sqrs else
  if (size = 2) then
    let upward_sqrs = minimize2 (emptySqrs_up b (y,x)) in
    let downward_sqrs = minimize2 (emptySqrs_down b (y,x)) in
    List.rev downward_sqrs@upward_sqrs else
  if (size = 1) then
    let upward_sqrs = minimize1 (emptySqrs_up b (y,x)) in
    let downward_sqrs = minimize1 (emptySqrs_down b (y,x)) in
    List.rev downward_sqrs@upward_sqrs else raise InvalidSize

(** [horizontal_tile_placements b c s] is the list of coordinates for possible
    horizontal tile placements anchored on coordinate [c] on [b] that one can
    make. The length of the list is limited to [2 * size]. Raises InvalidSize
    if [size] > 7 or [size] < 1. Arranged from left to right.
    Example: [(6,7), (8,7), (9,7)], anchored on a board with only a single Final
    tile placed on (7,7).*)
let horizontal_tile_placements b (y,x) size: (int * int) list =
  if (size > 7) then raise InvalidSize else
  if (size = 7) then
    let leftward_sqrs = minimize7 (emptySqrs_left b (y,x)) in
    let rightward_sqrs = minimize7 (emptySqrs_right b (y,x)) in
    List.rev leftward_sqrs@rightward_sqrs else
  if (size = 6) then
    let leftward_sqrs = minimize6 (emptySqrs_left b (y,x)) in
    let rightward_sqrs = minimize6 (emptySqrs_right b (y,x)) in
    List.rev leftward_sqrs@rightward_sqrs else
  if (size = 5) then
    let leftward_sqrs = minimize5 (emptySqrs_left b (y,x)) in
    let rightward_sqrs = minimize5 (emptySqrs_right b (y,x)) in
    List.rev leftward_sqrs@rightward_sqrs else
  if (size = 4) then
    let leftward_sqrs = minimize4 (emptySqrs_left b (y,x)) in
    let rightward_sqrs = minimize4 (emptySqrs_right b (y,x)) in
    List.rev leftward_sqrs@rightward_sqrs else
  if (size = 3) then
    let leftward_sqrs = minimize3 (emptySqrs_left b (y,x)) in
    let rightward_sqrs = minimize3 (emptySqrs_right b (y,x)) in
    List.rev leftward_sqrs@rightward_sqrs else
  if (size = 2) then
    let leftward_sqrs = minimize2 (emptySqrs_left b (y,x)) in
    let rightward_sqrs = minimize2 (emptySqrs_right b (y,x)) in
    List.rev leftward_sqrs@rightward_sqrs else
  if (size = 1) then
    let leftward_sqrs = minimize1 (emptySqrs_left b (y,x)) in
    let rightward_sqrs = minimize1 (emptySqrs_right b (y,x)) in
    List.rev leftward_sqrs@rightward_sqrs else raise InvalidSize


(** [insert_all_positions x l] is the list of all possible arrangements for
    inserting [x] in [l], which is a list of permutations. *)
let insert_all_positions x list =
  let rec aux p acc = function
    | [] -> (p @ [x])::acc |>  List.rev
    | hd::tl as l -> aux (p @ [hd]) ((p @ [x] @ l) :: acc) tl in
  aux [] [] list

(** [permutations l] is the list of all possible permutations of elements in [l].*)
let rec permutations = function
  | [] -> []
  | x::[] -> [[x]]
  | x::xs -> List.fold_left (fun acc p -> acc @ insert_all_positions x p) []
               (permutations xs)

(** [combiniations l k] is the size [k] list of all potential combinations of elements in [l].*)
let combinations list k =
  let rec helper1 l k acc =
    if k<=0 || k > List.length l then acc []
    else if k = 1 then acc (List.map (fun x -> [x]) l)
    else
      let hd, tl = List.hd l, List.tl l in
      helper1 tl k
        (
          fun helper2 -> helper1 tl (k-1)
              (
                fun helper3 -> acc (List.rev_append (List.map (fun x -> hd::x) helper3) helper2)
              )
        )
  in helper1 list k (fun x -> x)

(* [totalcombinations l s a] is the list of all permutations of elements in [l]
   of size [s] and up to the length of [l].
   Example: should return a list of size 13699, given [l] has seven elements, [s] is one,
   and [a] is the empty list. *)
let rec totalcombinations list startSize acclist=
  if (startSize <= (List.length list)) then
    let combos = combinations list startSize in
    let rec perms cmb accList =
      match cmb with
      | h::t -> perms t ((permutations h)@accList)
      | _ -> accList in
    let indexPerms = (perms combos []) in
    totalcombinations list (startSize + 1) (indexPerms @ acclist)
  else acclist

(** [window_startSizes b c] is the int*int tuple representing start window
    sizes for probing potential tile placements around coordinate [c] on [b], with
    the first int representing the horizontal window size, and the second int
    representing the vertical window size. The window sizes are capped at 7. *)
let window_startSizes board (y,x): (int * int) =
  let vertical = vertical_tile_placements board (y,x) 7 in
  let horizontal = horizontal_tile_placements board (y,x) 7 in
  if ((List.length vertical) >= 7) && ((List.length horizontal) >= 7) then
    (7,7) else if ((List.length vertical) < 7) && ((List.length horizontal) >= 7) then
    (List.length vertical, 7) else if ((List.length vertical) >= 7) && ((List.length horizontal) < 7) then
    (7, List.length horizontal) else (List.length horizontal, List.length vertical)

(** [final_tile_coords board] is the list of all coordinates of final tiles on
    [board]. No particular ordering.*)
let final_tile_coords board =
  let rec helper index acclist =
    if index < 15 then begin
      let rec helper2 col ypos acclist2 =
        match (col: square list) with
        | (Final a, _):: t -> helper2 t (ypos+1) ((index, ypos)::acclist2)
        | (a, _)::t -> helper2 t (ypos+1) (acclist2)
        | [] -> List.rev acclist2 in
      let col_squares = helper2 (List.nth board index) 0 [] in
      helper (index + 1) (col_squares::acclist) end
    else (List.rev acclist) in
  List.flatten (helper 0 [])

(** [finalTiles_windowSizes b tl] is the associative list of int tuples. It
    associates the elements of [tl], which are coordinates of final tiles on [b],
    with their window start sizes. *)
let finalTiles_windowSizes board tileList =
  let rec helper tiles (acclist: ((int*int) * (int*int)) list) =
    match tiles with
    | h::t -> helper t ((h,(window_startSizes board h))::acclist)
    | _ -> acclist in
  helper tileList []

(** [permutate l n] is the list of all [n]-size permutations of [l].*)
let permutate (list: 'a list) n: 'a list list =
  let combos = combinations list n in
  let rec helper cbs accList =
    match cbs with
    | h::t -> helper t ((permutations h)@accList)
    | _ -> accList
  in helper combos []

let remove_first_element list =
  match list with
  | h::t -> t
  | _ -> failwith "remove_first_element: empty input list"

(** [segment l n] is the in-order list of [n]-size partitions of [l].
    Example: [segment [1;2;3;4] 3] is [[1;2;3];[2;3;4]]. *)
let segment list n =
  let rec helper1 lis accList1 =
    if (List.length lis >= n) then
      let rec helper2 l k accList2=
        if (k <= n ) then
          match l with
          |h::t -> helper2 t (k+1) (h::accList2)
          |_ -> List.rev accList2
        else List.rev accList2 in
      let lists = ((helper2 lis 1 [])::accList1) in
      helper1 (remove_first_element lis) lists
    else List.rev accList1 in
  helper1 list []

(** [first_valid_tiles s l] is the FIRST valid list of Actions, from [l]. That is,
    the list of Actions generates a state from [s] with all tile placements being valid
    and all strings formed being valid words. Returns an empty list if there isn't
    a valid list of Actions. *)
let first_valid_tiles state (lists: Actions.action list list) =
  let rec helper1 l acclist=
    match (l,acclist) with
    | _, actions::xs -> acclist
    | h::t, [] ->
      let rec helper2 st moves =
        match moves with
        | Place (letter,pos)::xs -> let new_state = place_tile st (letter,pos) in
          helper2 new_state xs
        | _ -> try (match (calc_score st.board) with
            |(_,_) -> h) with
        |InvalidWord string -> [] in
      helper1 t ((helper2 state h)@acclist)
    | _ -> []
  in helper1 lists []

(** [valid_tiles s list] determines if [list] produces valid tile placements
    and valid words, given current state [s]. Returns an empty list if there
    isn't a valid list of Actions. *)
let valid_tiles state list =
  let rec helper st moves =
    match moves with
    | Place (letter,pos)::xs -> let new_state = place_tile st (letter,pos) in
      helper new_state xs
    | _ -> begin try (match (calc_score st.board) with
        |(_,_) -> list) with 
      |InvalidWord s -> [] 
      |InvalidTilePlacement -> [] end in helper state list 

(** [dock_letters d] is the string list of all letters on a given [d].*)
let dock_letters dock =
  [(List.nth dock 0).letter; (List.nth dock 1).letter;
   (List.nth dock 2).letter; (List.nth dock 3).letter;
   (List.nth dock 4).letter; (List.nth dock 5).letter;
   (List.nth dock 6).letter]

let ai_actions (cur_st:Moment.t): Actions.action list =
  (* setting up *)
  let cur_brain = {original_state = cur_st;
                   hypothetical_state = cur_st;
                   actions = []} in
  let orig_board = cur_brain.original_state.board in
  let dock = cur_brain.original_state.current_player.dock in
  let fT_wS = finalTiles_windowSizes cur_brain.original_state.board
      (final_tile_coords cur_brain.original_state.board) in

  if (is_firstmove (cur_st.board)) then
    let all_perms = totalcombinations dock 3 [] in
    (* (1) begin iterating through all permutations of the AI's tiles*)
    let rec halper1 perms =
      match perms with
      |list::xs -> let rec halper2 perm index acclist =
                     match perm with
                     | h::t -> halper2 t (index+1) (Place (h.letter, (7 + index, 7))::acclist)
                     | _ ->  let possible_actions = List.rev acclist in
                       match valid_tiles cur_brain.original_state possible_actions with
                       | [] -> (false,[])
                       | _ -> (true,possible_actions) in
        let potential_move = halper2 list 0 [] in
        if (fst potential_move) then snd potential_move else
          halper1 xs
      |_ -> [] in
    let possibleActions = halper1 all_perms in
    let valid_tile_placement = valid_tiles cur_brain.original_state possibleActions in
    match valid_tile_placement with
    | [] -> [Exchange (dock_letters dock)]
    | _ -> (List.rev (End::valid_tile_placement))
  else


    (* (1) begin iterating through each final tile **)
    let rec vert_possible_placements (start: int) (limit:int) (ft_ws: ((int*int) * (int*int)) list) (cmd0: action list) = 
      if cmd0 != [] then cmd0 else 
        match ft_ws with 
        | ((y,x),(vert,hor))::t -> begin 

            (* (2) begin iterating through all window sizes for tile at hand *)
            let rec helper2 windowSize cmd1 =
              if cmd1 != [] then cmd1 else

                (* Change the 2nd "windowSize guard statement" to limit number
                   of possible tiles AI can place in a move.*)

              if (windowSize <= vert) && (windowSize <= limit) then 
                let perms = permutate dock (windowSize) in

                (* (3) begin iterating through all possible lists of positions for window size at hand *)
                let rec helper3 positions cmd2 =
                  if cmd2 != [] then cmd2 else
                    match positions with
                    | pos1::t ->

                      (* (4) begin iterating through all possible permutations for list of positions at hand *)
                      let rec helper4 pos1 permIndex : Actions.action list =
                        if (permIndex < (List.length perms)) then

                          let current_perm = List.nth perms permIndex in

                          (* (5) construct action list for permutation at hand *)
                          let rec helper5 pos2 perm_items list2 =

                            match (pos2, perm_items) with
                            | (c1::t1,c2::t2) -> helper5 t1 t2 ((Place ((c2.letter), c1))::list2)
                            | _ -> let possible_actions = List.rev list2 in
                              match valid_tiles cur_brain.original_state possible_actions with
                              | [] -> (false,[])
                              | _ -> (true, possible_actions) in

                          let potential_move = helper5 pos1 current_perm [] in
                          if (fst potential_move) then (snd potential_move) else
                            helper4 pos1 (permIndex + 1)
                        else [] in

                      helper3 t ((helper4 pos1 1))
                    | _ -> cmd2 in

                let actions_for_window = ((helper3 (segment(vertical_tile_placements orig_board (y,x) windowSize) windowSize) [])) in
                helper2 (windowSize + 1) actions_for_window
              else cmd1 in

            let actions_for_all_windows = helper2 start [] in 
            vert_possible_placements start (limit) t (actions_for_all_windows)
          end
        | _ -> cmd0 in

    let rec hor_possible_placements (start:int) (limit:int) (ft_ws: ((int*int) * (int*int)) list) (cmd0: action list) = 
      if cmd0 != [] then cmd0 else 
        match ft_ws with 
        | ((y,x),(vert,hor))::t -> begin 

            (* (2) begin iterating through all window sizes for tile at hand *)
            let rec helper2 windowSize cmd1 =
              if cmd1 != [] then cmd1 else

                (* Change the 2nd "windowSize guard statement" to limit number
                   of possible tiles AI can place in a move.*)

              if (windowSize <= hor) && (windowSize <= limit) then 
                let perms = permutate dock (windowSize) in

                (* (3) begin iterating through all possible lists of positions for window size at hand *)
                let rec helper3 positions cmd2 =
                  if cmd2 != [] then cmd2 else
                    match positions with
                    | pos1::t ->

                      (* (4) begin iterating through all possible permutations for list of positions at hand *)
                      let rec helper4 pos1 permIndex : Actions.action list =
                        if (permIndex < (List.length perms)) then

                          let current_perm = List.nth perms permIndex in

                          (* (5) construct action list for permutation at hand *)
                          let rec helper5 pos2 perm_items list2 =

                            match (pos2, perm_items) with
                            | (c1::t1,c2::t2) -> helper5 t1 t2 ((Place ((c2.letter), c1))::list2)
                            | _ -> let possible_actions = List.rev list2 in
                              match valid_tiles cur_brain.original_state possible_actions with
                              | [] -> (false,[])
                              | _ -> (true, possible_actions) in

                          let potential_move = helper5 pos1 current_perm [] in
                          if (fst potential_move) then (snd potential_move) else
                            helper4 pos1 (permIndex + 1)
                        else [] in

                      helper3 t ((helper4 pos1 1))
                    | _ -> cmd2 in

                let actions_for_window = ((helper3 (segment(horizontal_tile_placements orig_board (y,x) windowSize) windowSize) [])) in
                helper2 (windowSize + 1) actions_for_window
              else cmd1 in

            let actions_for_all_windows = helper2 start [] in 
            hor_possible_placements start limit t (actions_for_all_windows)
          end
        | _ -> cmd0 in

    let small_hor_tile_placements = hor_possible_placements 2 3 fT_wS [] in 
    match small_hor_tile_placements with
    | [] -> begin 
        let small_vert_tile_placements = vert_possible_placements 2 3 fT_wS [] in
        match small_vert_tile_placements with
        | [] -> begin 
            let med_hor_tile_placements = hor_possible_placements 4 5 fT_wS [] in 
            match med_hor_tile_placements with 
            | [] -> begin 
                let med_vert_tile_placements = vert_possible_placements 4 5 fT_wS [] in 
                match med_vert_tile_placements with 
                | [] -> 
                  begin 
                    let large_hor_tile_placements = hor_possible_placements 6 6 fT_wS [] in 
                    match large_hor_tile_placements with 
                    | [] -> begin 
                        let large_vert_tile_placements = vert_possible_placements 6 6 fT_wS [] in 
                        match large_vert_tile_placements with 
                        | [] -> [Exchange (dock_letters dock)]
                        | _ ->  (List.rev (End::large_vert_tile_placements))
                      end
                    | _ ->  (List.rev (End::large_hor_tile_placements))
                  end
                | _ -> (List.rev (End::med_vert_tile_placements))


              end
            | _ -> (List.rev (End::med_hor_tile_placements))

          end
        | _ -> (List.rev (End::small_vert_tile_placements))
      end
    | _ -> (List.rev (End::small_hor_tile_placements))
