open OUnit2
open Actions
open Moment
open Board

let make_parse_tests
    (name: string)
    (str : string)
    (exp_cmd : Actions.action) =
  name >:: (fun _ ->
      assert_equal exp_cmd (Actions.parse_cmd str);
    )
let make_parse_failures
    (name: string)
    (str : string)
    (error : exn) =
  name >:: (fun _ -> assert_raises error (fun () -> Actions.parse_cmd str)
           )

let make_placement_tests 
    (name: string)
    (state: Moment.t)
    (error: exn) = 
  name >:: (fun _ -> assert_raises error (fun () -> Board.calc_score (Moment.get_board state)))

let make_exchange_failure_tests 
    (name: string)
    (state: Moment.t) 
    (strings: string list)
    (error: exn)=
  name >:: (fun _ -> assert_raises error (fun () -> exchange state strings))

let exchange_state_1 = 
  let new_player = {
    name = "Test1";
    dock = [{letter = "A"; value = 1};
            {letter = "A"; value = 1};
            {letter = "A"; value = 1};
            {letter = "A"; value = 1};
            {letter = "A"; value = 1};
            {letter = "A"; value = 1}];
    score = 0
  } in
  {
    board = emptyBoard;
    bag = [];
    players = [new_player];
    current_player = new_player;
    log = []
  }

let exchange_state_2 = 
  let new_player = {
    name = "Test2";
    dock = [{letter = "A"; value = 1};
            {letter = "B"; value = 1};
            {letter = "C"; value = 1};
            {letter = "D"; value = 1};
            {letter = "E"; value = 1};
            {letter = "F"; value = 1};
            {letter = "G"; value = 1}];
    score = 0
  } in
  {
    board = emptyBoard;
    bag = [];
    players = [new_player];
    current_player = new_player;
    log = []
  }

let placement_state_1 = 
  let new_player = {
    name = "Test1";
    dock = [{letter = "B"; value = 1};
            {letter = "A"; value = 1};
            {letter = "T"; value = 1};
            {letter = "D"; value = 1};
            {letter = "Z"; value = 1};
            {letter = "F"; value = 1};
            {letter = "G"; value = 1}];
    score = 0;
  } in
  {
    board = emptyBoard;
    bag = init_bag;
    players = [new_player];
    current_player = new_player;
    log = [];
  }

let placement_state_2 = 
  let new_player = {
    name = "Test2";
    dock = [{letter = "H"; value = 1};
            {letter = "W"; value = 1};
            {letter = "A"; value = 1};
            {letter = "D"; value = 1};
            {letter = "O"; value = 1};
            {letter = "R"; value = 1};
            {letter = "N"; value = 1}];
    score = 0;
  } in
  {
    board = emptyBoard;
    bag = init_bag;
    players = [new_player];
    current_player = new_player;
    log = [];
  }


let pipeline_place tuple state = place_tile state tuple

let place_tuple letter row col = (letter, (single_to_int row, int_of_string col)) 


let end_turn_1 = 
  let new_player = {
    name = "Test1";
    dock = [
      {letter = "W"; value = 1};
      {letter = "A"; value = 1};
      {letter = "D"; value = 1};
      {letter = "H"; value = 1};
      {letter = "O"; value = 1};
      {letter = "W"; value = 1};
      {letter = "N"; value = 1};
    ];
    score = 0;
  } in
  {
    board = emptyBoard;
    bag = init_bag;
    players = [new_player];
    current_player = new_player;
    log = [];
  }

let make_score_tests
    (name: string)
    (state : Moment.t)
    (exp_score : int) =
  name >:: (fun _ ->
      Words.add_hash_set Words.word_set Words.word_array Hashtbl.hash;
      assert_equal exp_score (get_current_player state).score; )


let rule_tests = 
  [
    make_placement_tests "First tile is not h7" 
      (place_tile placement_state_1 ("A",(single_to_int "H", int_of_string "8")))
      InvalidTilePlacement; 
    make_placement_tests "Invalid Word" 
      (place_tile (place_tile placement_state_1 ("A",(single_to_int "H", int_of_string "7")))
         ("Z",(single_to_int "H", int_of_string "8")))
      (InvalidWord "AZ"); 
    make_placement_tests "Non-connected placement" 
      (place_tile (place_tile placement_state_1 ("A",(single_to_int "H", int_of_string "7")))
         ("T",(single_to_int "M", int_of_string "0")))
      (InvalidTilePlacement); 
    make_placement_tests "Triangular Placement" 
      (place_tile (place_tile (place_tile placement_state_1 
                                 ("A",(single_to_int "H", int_of_string "7")))
                     ("T",(single_to_int "H", int_of_string "8")))
         ("B", (single_to_int "G", int_of_string "7")))
      (InvalidTilePlacement); 
    make_exchange_failure_tests "Exchange with other than 7 tiles" 
      exchange_state_1 ["A"] InvalidExchange;
    make_exchange_failure_tests "Exchange with nonexistant tile" 
      exchange_state_2 ["H";"I"] MissingTilesToExchange;
    make_exchange_failure_tests "Invalid Character String" 
      exchange_state_2 ["HI"] MissingTilesToExchange;
    make_placement_tests "Valid One Direction, Invalid One Direction"
      ((placement_state_2) |> pipeline_place (place_tuple "W" "H" "7") 
       |> pipeline_place (place_tuple "A" "H" "8") 
       |> pipeline_place (place_tuple "D" "H" "9") 
       |> pipeline_place (place_tuple "H" "H" "6")
       |> pipeline_place (place_tuple "O" "I" "6")
       |> pipeline_place (place_tuple "R" "J" "6")
       |> pipeline_place (place_tuple "N" "K" "6"))
      (InvalidTilePlacement); (* InvalidWord "HWAD"*)

    (* make_score_tests "Score" 
       (fst (play_word (end_turn_1 
                       |> pipeline_place (place_tuple "H" "H" "7") 
                       |> pipeline_place (place_tuple "O" "I" "7") 
                       |> pipeline_place (place_tuple "W" "J" "7")))) 
       6; *)



  ]

let action_tests =
  [
    make_parse_failures "1" "aasd asdsa" Broken;
    make_parse_failures "2" "" Blank;
    make_parse_tests "3" "place a o 10" (Actions.Place ("A",(single_to_int "O",10)));
    make_parse_tests "4" "quit" (Actions.Quit);
    make_parse_failures "5" "place duf" Broken;
    make_parse_failures "6" "placeasd" Broken;
    make_parse_failures "7" " " Blank;
    make_parse_failures "8" " place a o" Broken;
    make_parse_failures "9" "exchange" Broken;
    make_parse_failures "10" "exchange 20" BadSelection;
  ]


let test_empty_set_1 : (int Words.StringHashTbl.t) =
  Words.StringHashTbl.create (3)
let test_empty_set_2 : (int Words.StringHashTbl.t) =
  Words.StringHashTbl.create (3)
let test_empty_set_3 : (int Words.StringHashTbl.t) =
  Words.StringHashTbl.create (3)

let make_add_set_tests
    (name: string)
    (arr: string array)
    (set: int Words.StringHashTbl.t)
    (exp: int Words.StringHashTbl.t) =
  name >:: (fun _ ->
      (Words.add_hash_set set arr Hashtbl.hash);
      Words.StringHashTbl.add test_empty_set_2 "1" (Hashtbl.hash "1");
      Words.StringHashTbl.add test_empty_set_2 "2" (Hashtbl.hash "2");
      Words.StringHashTbl.add test_empty_set_2 "3" (Hashtbl.hash "3");
      assert_equal exp set;
    )

let make_validity_tests
    (name: string)
    (str: string)
    (set: int Words.StringHashTbl.t)
    (exp: bool) =
  name >:: (fun _ ->
      Words.add_hash_set test_empty_set_3 [|"4"; "5"; "6"|] Hashtbl.hash;
      assert_equal exp (Words.validity str set);
    )

let word_tests =
  [
    make_add_set_tests "1" [|"1"; "2"; "3"|] test_empty_set_1 test_empty_set_2;
    make_validity_tests "valid 1" "4" test_empty_set_3 true;
    make_validity_tests "valid 2" "5" test_empty_set_3 true;
    make_validity_tests "valid 3" "1" test_empty_set_3 false;
    make_validity_tests "valid 4" "99" test_empty_set_3 false;
    make_validity_tests "valid 5" "6" test_empty_set_3 true;
  ]

let make_refill_tests
    (name: string)
    (state: Moment.t)
    (exp: int) =
  name >:: (fun _ ->
      assert_equal (List.length state.current_player.dock)
        exp;
    )

let state_3_elements : Moment.t = {
  board = Board.emptyBoard;
  bag = Moment.init_bag;
  players = [
    {
      name = "OScrabl Player";
      dock =
        [{letter = "A"; value = 1};
         {letter = "A"; value = 1};
         {letter = "A"; value = 1};];
      score = 0;
    }
  ];
  current_player = {
    name = "OScrabl Player";
    dock =
      [{letter = "A"; value = 1};
       {letter = "A"; value = 1};
       {letter = "A"; value = 1};];
    score = 0;
  };
  log = [];
}
let state_0_elements : Moment.t = {
  board = Board.emptyBoard;
  bag = Moment.init_bag;
  players = [
    {
      name = "OScrabl Player";
      dock =
        [];
      score = 0;
    }
  ];
  current_player = {
    name = "OScrabl Player";
    dock =
      [];
    score = 0;
  };
  log = [];
}
let state_7_elements : Moment.t = {
  board = Board.emptyBoard;
  bag = Moment.init_bag;
  players = [
    {
      name = "OScrabl Player";
      dock =
        [{letter = "A"; value = 1};
         {letter = "A"; value = 1};
         {letter = "A"; value = 1};
         {letter = "A"; value = 1};
         {letter = "A"; value = 1};
         {letter = "A"; value = 1};
         {letter = "A"; value = 1};];
      score = 0;
    }
  ];
  current_player = {
    name = "OScrabl Player";
    dock =
      [{letter = "A"; value = 1};
       {letter = "A"; value = 1};
       {letter = "A"; value = 1};
       {letter = "A"; value = 1};
       {letter = "A"; value = 1};
       {letter = "A"; value = 1};
       {letter = "A"; value = 1};];
    score = 0;
  };
  log = [];
}
let moment_tests =
  [
    (* make_refill_tests "Between 0 and 7 tiles"
       (Moment.refill state_3_elements;) 7;
       make_refill_tests "empty dock to start"
       (Moment.refill state_0_elements;) 7;
       make_refill_tests "full dock to start"
       (Moment.refill state_7_elements;) 7; *)
  ]

let suite =
  "OScrabl test suite"  >::: List.flatten [
    action_tests;
    word_tests;
    moment_tests;
    rule_tests;
  ]

let _ = run_test_tt_main suite
