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


let suite =
  "OScrabl test suite"  >::: List.flatten [
    action_tests;
  ]

let _ = run_test_tt_main suite