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
      input_list |> Odash.drop(drop_size) |> assert_equal(input_list);
    },
    "drop of a negative int returns a list as-is" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let drop_size = -99;
      input_list |> Odash.drop(drop_size) |> assert_equal(input_list);
    },
    "some returns true if one item meets criteria" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let test_func = i => i > 10;
      let expected_output = true;
      input_list |> Odash.some(test_func) |> assert_equal(expected_output);
    },
    "some returns true if all items meet criteria" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let test_func = i => i > -1;
      let expected_output = true;
      input_list |> Odash.some(test_func) |> assert_equal(expected_output);
    },
    "some returns false if no items meet criteria" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let test_func = i => i < 0;
      let expected_output = false;
      input_list |> Odash.some(test_func) |> assert_equal(expected_output);
    },
    "some returns false if input list is empty" >:: () => {
      let input_list = [];
      let test_func = _ => true;
      let expected_output = false;
      input_list |> Odash.some(test_func) |> assert_equal(expected_output);
    },
    "some returns true if one item meets criteria" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let test_func = i => i > 10;
      let expected_output = true;
      input_list |> Odash.some(test_func) |> assert_equal(expected_output);
    },
    "difference returns items in first list not in second" >:: () => {
      let starting_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let comparison_list = [0,2,4,6,8,10,12];
      let expected_output = [1,3,5,7,9,11];
      starting_list |> Odash.difference(comparison_list) |> assert_equal(expected_output);
    },
    "difference returns items in first list not in second, matching by ==" >:: () => {
      let starting_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let comparison_list = [0,2,4,6,8,10,12];
      let expected_output = [1,3,5,7,9,11];
      starting_list |> Odash.difference(comparison_list) |> assert_equal(expected_output);
    },
    "difference returns first list as-is if no matches in second list" >:: () => {
      let starting_list = [0.0,1.1,2.2,3.3,4.4];
      let comparison_list = [6.6,8.8,10.10,12.12];
      starting_list |> Odash.difference(comparison_list) |> assert_equal(starting_list);
    },
    "difference returns empty list if all match with second list" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let comparison_list = ['a','b','c','d','e','f'];
      let expected_output = [];
      starting_list |> Odash.difference(comparison_list) |> assert_equal(expected_output);
    },
    "differenceBy returns items in first list not in second, matching by comparison_func" >:: () => {
      let starting_list = [-1,3,-4,-7];
      let comparison_list = [1,-2,-4];
      let comparison_func = (n) => Pervasives.abs(n)
      let expected_output = [3,-7];
      starting_list |> Odash.differenceBy(comparison_func, comparison_list) |> assert_equal(expected_output);
    },
];

run_test_tt_main(suite);
