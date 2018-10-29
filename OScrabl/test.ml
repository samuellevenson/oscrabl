open OUnit2
open Actions


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
  ]
let suite =
  "OScrabl test suite"  >::: List.flatten [
    action_tests;
    word_tests;
  ]

let _ = run_test_tt_main suite