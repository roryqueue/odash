open OUnit;
open Lib;


let suite =
  "Odash" >::: [
    "chunk splits evenly divisible list into list of lists" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let chunk_size = 3;
      let expected_output = [[0,1,2],[3,4,5],[6,7,8],[9,10,11]];
      input_list |> Odash.chunk(chunk_size) |> assert_equal(expected_output);
    },
    "chunk splits not-evenly-divisible list with remainder in last sublist" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10];
      let chunk_size = 3;
      let expected_output = [[0,1,2],[3,4,5],[6,7,8],[9,10]];
      input_list |> Odash.chunk(chunk_size) |> assert_equal(expected_output);
    },
    /* TODO: figure out why this doesn't type check
    "chunk raises invalid if chunk_size is negative" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let chunk_size = -3;
      let expected_exception = Odash.Invalid("chunk_size must be a positive integer!");
      input_list |> Odash.chunk(chunk_size)  |> assert_raises(expected_exception);
    }, */
    "drop n returns a list with n items dropped from the beginning" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let drop_size = 3;
      let expected_output = [3,4,5,6,7,8,9,10,11];
      input_list |> Odash.drop(drop_size) |> assert_equal(expected_output);
    },
    "drop 0 returns a list as-is" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let drop_size = 0;
      let expected_output = [0,1,2,3,4,5,6,7,8,9,10,11];
      input_list |> Odash.drop(drop_size) |> assert_equal(expected_output);
    },
    "drop of a negative int returns a list as-is" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let drop_size = -99;
      let expected_output = [0,1,2,3,4,5,6,7,8,9,10,11];
      input_list |> Odash.drop(drop_size) |> assert_equal(expected_output);
    },
];

run_test_tt_main(suite);
