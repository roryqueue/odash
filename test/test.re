open OUnit;
/* open Lib; */


let suite =
  "Odash" >::: [
    "equality works" >:: () => {
      1 === 1 |> assert_equal(true);
    }
];

run_test_tt_main(suite);
