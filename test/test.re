open OUnit;
open Lib;

let suite =
  "Odash" >::: [
    "map applies a function to each member of a list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let map_func = (_, _, i) => i + 1;
      let expected_output = [1,2,3,4,5,6,7,8,9,10,11,12];
      input_list |> Odash.map(map_func) |> assert_equal(expected_output);
    },
    "map can apply a function using starting list passed as first argument to map_func" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let map_func = (l, _, i) => Odash.includes(i + 5, l) ? i + 5 : i;
      let expected_output = [5,6,7,8,9,10,11,7,8,9,10,11];
      input_list |> Odash.map(map_func) |> assert_equal(expected_output);
    },
    "map can apply a function using item index passed as second argument to map_func" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let map_func = (_, idx, item) => idx + item;
      let expected_output = [0,2,4,6,8,10,12,14,16,18,20,22];
      input_list |> Odash.map(map_func) |> assert_equal(expected_output);
    },
    "map passes through an empty list unchanged" >:: () => {
      let input_list = [];
      let map_func = (_, idx, item) => idx + item;
      input_list |> Odash.map(map_func) |> assert_equal(input_list);
    },
    "flatten flattens a list of lists (by one level of depth)" >:: () => {
      let input_list = [[0,1],[2],[3],[4,5],[6,7,8,9],[10,11]];
      let expected_output = [0,1,2,3,4,5,6,7,8,9,10,11];
      input_list |> Odash.flatten |> assert_equal(expected_output);
    },
    "flatten passes through an empty list unchanged" >:: () => {
      let input_list = [];
      input_list |> Odash.flatten |> assert_equal(input_list);
    },
    "flatMap applies a function then flattens the results" >:: () => {
      let input_list = [0,1,2,3,4];
      let map_func = (_, idx, item) => [idx, item];
      let expected_output = [0,0,1,1,2,2,3,3,4,4];
      input_list |> Odash.flatMap(map_func) |> assert_equal(expected_output);
    },
    "flatMap passes through an empty list unchanged" >:: () => {
      let input_list = [];
      let map_func = (_, idx, item) => [idx, item];
      input_list |> Odash.flatMap(map_func) |> assert_equal(input_list);
    },
    "identity returns its argument" >:: () => {
      let a_number = 1;
      let a_string = "hi";
      let a_list = [1,4];
      a_number |> Odash.identity |> assert_equal(a_number);
      a_string |> Odash.identity |> assert_equal(a_string);
      a_list |> Odash.identity |> assert_equal(a_list);
    },
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
      input_list |> Odash.chunk(chunk_size) |> assert_raises(expected_exception);
    }, */
    "dropWhile applies the provided func to the next item, index, and rest of list, and drops until false" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, i) => i < 5;
      let expected_output = [5,6,7,8,9,10,11];
      input_list |> Odash.dropWhile(while_func) |> assert_equal(expected_output);
    },
    "dropWhile can operate on item and full list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,3];
      let while_func = (list, _, next_item) => {
        list
        |> List.filter((i) => i == next_item)
        |> List.length == 1;
      };
      let expected_output = [3,4,5,6,7,8,9,10,3];
      input_list |> Odash.dropWhile(while_func) |> assert_equal(expected_output);
    },
    "dropWhile returns list unaltered if provided func is false for the first item" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, _) => false;
      input_list |> Odash.dropWhile(while_func) |> assert_equal(input_list);
    },
    "dropWhile returns empty list if provided func is true for all items" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, _) => true;
      let expected_output = [];
      input_list |> Odash.dropWhile(while_func) |> assert_equal(expected_output);
    },
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
    "dropRightWhile applies the provided func to the last item, reverse index, and rest of list, and drops until false" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, i) => i > 5;
      let expected_output = [0,1,2,3,4,5];
      input_list |> Odash.dropRightWhile(while_func) |> assert_equal(expected_output);
    },
    "dropRight n returns a list with n items dropped from the end" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let drop_size = 3;
      let expected_output = [0,1,2,3,4,5,6,7,8];
      input_list |> Odash.dropRight(drop_size) |> assert_equal(expected_output);
    },
    "takeWhile applies the provided func to the next item, index, and rest of list, and keeps until false" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, i) => i < 5;
      let expected_output = [0,1,2,3,4];
      input_list |> Odash.takeWhile(while_func) |> assert_equal(expected_output);
    },
    "takeWhile can operate on item and full list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,7];
      let while_func = (list, _, next_item) => {
        list
        |> List.filter((i) => i == next_item)
        |> List.length == 1;
      };
      let expected_output = [0,1,2,3,4,5,6];
      input_list |> Odash.takeWhile(while_func) |> assert_equal(expected_output);
    },
    "takeWhile returns an empty list if provided func is false for the first item" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, _) => false;
      let expected_output = [];
      input_list |> Odash.takeWhile(while_func) |> assert_equal(expected_output);
    },
    "takeWhile a list unaltered if provided func is true for all items" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, _) => true;
      input_list |> Odash.takeWhile(while_func) |> assert_equal(input_list);
    },
    "take n returns a list with n items dropped from the beginning" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let take_size = 3;
      let expected_output = [0,1,2];
      input_list |> Odash.take(take_size) |> assert_equal(expected_output);
    },
    "take 0 returns an empty list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let take_size = 0;
      let expected_output = [];
      input_list |> Odash.take(take_size) |> assert_equal(expected_output);
    },
    "take of a negative int returns an empty list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let take_size = -99;
      let expected_output = [];
      input_list |> Odash.take(take_size) |> assert_equal(expected_output);
    },
    "takeRightWhile applies the provided func to the last item, reverse index, and rest of list, and takes until false" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let while_func = (_, _, i) => i > 5;
      let expected_output = [6,7,8,9,10,11];
      input_list |> Odash.takeRightWhile(while_func) |> assert_equal(expected_output);
    },
    "takeRight n returns a list with n items taken from the end" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let take_size = 3;
      let expected_output = [9,10,11];
      input_list |> Odash.takeRight(take_size) |> assert_equal(expected_output);
    },
    "slice returns a sublist from start index to end index" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let start_idx = 3;
      let end_idx = 7;
      let expected_output = [3,4,5,6,7];
      input_list |> Odash.slice(start_idx, end_idx) |> assert_equal(expected_output);
    },
    "slice with same start and end returns a list of one" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let start_idx = 3;
      let end_idx = 3;
      let expected_output = [3];
      input_list |> Odash.slice(start_idx, end_idx) |> assert_equal(expected_output);
    },
    "slice with an end index before its start returns an empty list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let start_idx = 3;
      let end_idx = 2;
      let expected_output = [];
      input_list |> Odash.slice(start_idx, end_idx) |> assert_equal(expected_output);
    },
    "slice with a start index out of range returns an empty list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let start_idx = 32;
      let end_idx = 35;
      let expected_output = [];
      input_list |> Odash.slice(start_idx, end_idx) |> assert_equal(expected_output);
    },
    "slice with a negative start and end index returns an empty list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let start_idx = -7;
      let end_idx = -2;
      let expected_output = [];
      input_list |> Odash.slice(start_idx, end_idx) |> assert_equal(expected_output);
    },
    "slice with a negative end index but positive start indexreturns an empty list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let start_idx = 7;
      let end_idx = -2;
      let expected_output = [];
      input_list |> Odash.slice(start_idx, end_idx) |> assert_equal(expected_output);
    },
    "some returns true if one item meets criteria function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let test_func = (_, _, i) => i > 10;
      let expected_output = true;
      input_list |> Odash.some(test_func) |> assert_equal(expected_output);
    },
    "some returns true if all items meet criteria function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let test_func = (_, _, i) => i > -1;
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
      let comparison_func = n => Pervasives.abs(n)
      let expected_output = [3,-7];
      starting_list |> Odash.differenceBy(comparison_func, comparison_list) |> assert_equal(expected_output);
    },
    "fill fills in an array with the provided value" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let expected_output = ['f','f','f','f','f'];
      starting_list |> Odash.fill(fill_character) |> assert_equal(expected_output);
    },
    "fill can take an inclusive start index" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let start_idx = 2;
      let expected_output = ['a','b','f','f','f'];
      starting_list |> Odash.fill(~start_index=start_idx, fill_character) |> assert_equal(expected_output);
    },
    "fill replaces all array values if start index is zero" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let start_idx = 0;
      let expected_output = ['f','f','f','f','f'];
      starting_list |> Odash.fill(~start_index=start_idx, fill_character) |> assert_equal(expected_output);
    },
    "fill replaces all array values if start index is negative" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let start_idx = -100;
      let expected_output = ['f','f','f','f','f'];
      starting_list |> Odash.fill(~start_index=start_idx, fill_character) |> assert_equal(expected_output);
    },
    "fill doesn't replace any array values if start index is greater than or equal to list size" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let start_idx = 5;
      starting_list |> Odash.fill(~start_index=start_idx, fill_character) |> assert_equal(starting_list);
    },
    "fill can take an exclusive end index" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let end_idx = 2;
      let expected_output = ['f','f','c','d','e'];
      starting_list |> Odash.fill(~end_index=end_idx, fill_character) |> assert_equal(expected_output);
    },
    "fill doesn't replace any array values if end index is zero" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let end_idx = 0;
      starting_list |> Odash.fill(~end_index=end_idx, fill_character) |> assert_equal(starting_list);
    },
    "fill doesn't replace any array values if end index is negative" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let end_idx = -100;
      starting_list |> Odash.fill(~end_index=end_idx, fill_character) |> assert_equal(starting_list);
    },
    "fill replaces all array values if end index is greater than list size" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let end_idx = 6;
      let expected_output = ['f','f','f','f','f'];
      starting_list |> Odash.fill(~end_index=end_idx, fill_character) |> assert_equal(expected_output);
    },
    "fill can take both a start and an end index together" >:: () => {
      let starting_list = ['a','b','c','d','e'];
      let fill_character = 'f';
      let expected_output = ['a','b','f','f','e'];
      starting_list |> Odash.fill(~start_index=2, ~end_index=4, fill_character) |> assert_equal(expected_output);
    },
    "find returns Some of first element in a list that returns true for the find_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let find_func = (_, _, i) => i > 5;
      let expected_output = Some(6);
      input_list |> Odash.find(find_func) |> assert_equal(expected_output);
    },
    "find returns None for a list in which no element returns true for the find_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let find_func = (_, _, i) => i > 50;
      let expected_output = None;
      input_list |> Odash.find(find_func) |> assert_equal(expected_output);
    },
    "find returns Some of first element in a list if all elements return true for the find_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let find_func = (_, _, _) => true;
      let expected_output = Some(0);
      input_list |> Odash.find(find_func) |> assert_equal(expected_output);
    },
    "find returns None for an empty list" >:: () => {
      let input_list = [];
      let find_func = (_, _, _) => true;
      let expected_output = None;
      input_list |> Odash.find(find_func) |> assert_equal(expected_output);
    },
    "findLast returns Some of last element in a list that returns true for the find_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let find_func = (_, _, i) => i < 5;
      let expected_output = Some(4);
      input_list |> Odash.findLast(find_func) |> assert_equal(expected_output);
    },
    "findIndex returns Some of first element in a list that returns true for the find_function" >:: () => {
      let input_list = [10,11,12,13,14,15,16,17,18,19,20,21];
      let find_func = (_, _, i) => i > 15;
      let expected_output = 6;
      input_list |> Odash.findIndex(find_func) |> assert_equal(expected_output);
    },
    "findIndex returns -1 for a list in which no element returns true for the find_function" >:: () => {
      let input_list = [10,11,12,13,14,15,16,17,18,19,20,21];
      let find_func = (_, _, i) => i > 50;
      let expected_output = -1;
      input_list |> Odash.findIndex(find_func) |> assert_equal(expected_output);
    },
    "findIndex returns 0 if all elements return true for the find_function" >:: () => {
      let input_list = [10,11,12,13,14,15,16,17,18,19,20,21];
      let find_func = (_, _, _) => true;
      let expected_output = 0;
      input_list |> Odash.findIndex(find_func) |> assert_equal(expected_output);
    },
    "findIndex returns -1 for an empty list" >:: () => {
      let input_list = [];
      let find_func = (_, _, _) => true;
      let expected_output = -1;
      input_list |> Odash.findIndex(find_func) |> assert_equal(expected_output);
    },
    "findLastIndex returns index of last element in a list that returns true for the find_function" >:: () => {
      let input_list = [10,11,12,13,14,15,16,17,18,19,20,21];
      let find_func = (_, _, i) => i < 15;
      let expected_output = 4;
      let _ = input_list |> Odash.findLastIndex(find_func) |> print_int
      input_list |> Odash.findLastIndex(find_func) |> assert_equal(expected_output);
    },
    "some returns true if all elements in the list meets the some_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let some_func = (_, _, i) => i > -1;
      let expected_output = true;
      input_list |> Odash.some(some_func) |> assert_equal(expected_output);
    },
    "some returns true if any element in the list meets the some_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let some_func = (_, _, i) => i == 5;
      let expected_output = true;
      input_list |> Odash.some(some_func) |> assert_equal(expected_output);
    },
    "some returns false for a list in which no element returns true for the some_function" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let some_func = (_, _, i) => i > 50;
      let expected_output = false;
      input_list |> Odash.some(some_func) |> assert_equal(expected_output);
    },
    "some returns false for an empty list" >:: () => {
      let input_list = [];
      let some_func = (_, _, _) => true;
      let expected_output = false;
      input_list |> Odash.some(some_func) |> assert_equal(expected_output);
    },
    "every returns true if all elements of list meet every_func" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let every_func = (_, _, i) => i > -1;
      let expected_output = true;
      input_list |> Odash.every(every_func) |> assert_equal(expected_output);
    },
    "every returns false if only some elements of list meet every_func" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let every_func = (_, _, i) => i > 5;
      let expected_output = false;
      input_list |> Odash.every(every_func) |> assert_equal(expected_output);
    },
    "every returns false if no elements of list meet every_func" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let every_func = (_, _, i) => i > 50;
      let expected_output = false;
      input_list |> Odash.every(every_func) |> assert_equal(expected_output);
    },
    "includes returns true if element is in list once" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let element = 3;
      let expected_output = true;
      input_list |> Odash.includes(element) |> assert_equal(expected_output);
    },
    "includes returns true if list is composed of a bunch of specified element" >:: () => {
      let input_list = [3,3,3,3,3,3,3];
      let element = 3;
      let expected_output = true;
      input_list |> Odash.includes(element) |> assert_equal(expected_output);
    },
    "includes returns false if specified element is not in list" >:: () => {
      let input_list = [0,1,2,4,5,6,7,8,9,10,11];
      let element = 3;
      let expected_output = false;
      input_list |> Odash.includes(element) |> assert_equal(expected_output);
    },
    "includes returns false for empty list" >:: () => {
      let input_list = [];
      let element = 3;
      let expected_output = false;
      input_list |> Odash.includes(element) |> assert_equal(expected_output);
    },
    "partition splits a list in items true and false for predicate, preserving order" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let partition_func = i => (i mod 2) == 0;
      let expected_output = ([0,2,4,6,8,10], [1,3,5,7,9,11]);
      input_list |> Odash.partition(partition_func) |> assert_equal(expected_output);
    },
    "partition puts all items in first list if partition function true for all" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let partition_func = _ => true;
      let expected_output = ([0,1,2,3,4,5,6,7,8,9,10,11], []);
      input_list |> Odash.partition(partition_func) |> assert_equal(expected_output);
    },
    "partition puts all items in second list if partition function false for all" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let partition_func = _ => false;
      let expected_output = ([], [0,1,2,3,4,5,6,7,8,9,10,11]);
      input_list |> Odash.partition(partition_func) |> assert_equal(expected_output);
    },
    "partition returns two empty lists when given and empty list" >:: () => {
      let input_list = [];
      let partition_func = i => i mod 2 == 0;
      let expected_output = ([], []);
      input_list |> Odash.partition(partition_func) |> assert_equal(expected_output);
    },
    "sampleSize returns a random sample of the specified size from the list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let sample_size = 4;
      let sample = input_list |> Odash.sampleSize(sample_size)
      let _length_assertion = sample |> List.length |> assert_equal(sample_size);
      let all_include = (i) => Odash.includes(i, input_list) |> assert_equal(true)
      let _inclusion_assertion = sample |> List.map(all_include);
    },
    "sampleSize returns a shuffled list if the specified size is greater than the list size" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let sample_size = 100;
      let sample = input_list |> Odash.sampleSize(sample_size)
      let _length_assertion = sample |> List.length |> assert_equal(List.length(input_list));
      let all_include = (i) => Odash.includes(i, input_list) |> assert_equal(true)
      let _inclusion_assertion = sample |> List.map(all_include);
    },
    "sampleSize returns an empty list if given an empty list" >:: () => {
      let input_list = [];
      let sample_size = 1;
      input_list |> Odash.sampleSize(sample_size) |> assert_equal(input_list);
    },
    "sampleSize returns an empty list sample size specified as zero" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let sample_size = 0;
      let expected_output = [];
      input_list |> Odash.sampleSize(sample_size) |> assert_equal(expected_output);
    },
    "sample returns a random item from a list" >:: () => {
      let input_list = [0,1,2,3,4,5,6,7,8,9,10,11];
      let sample = Odash.sample(input_list)
      input_list |> Odash.includes(sample) |> assert_equal(true);
    },
    /* TODO: figure out why this doesn't type check
    "sample raises invalid if passed an empty list" >:: () => {
      let input_list = [];
      let expected_exception = "input_list cannot be empty!";
      Odash.sample(input_list) |> assert_raises(expected_exception);
    }, */
];

run_test_tt_main(suite);
