exception Invalid(string);
exception Impossible(string);

let rec drop: (int, list('a)) => list('a) =
  (count_to_drop, starting_list) => {
    switch (starting_list) {
    | [] => []
    | [_, ...list_dropping_one] as full_list => {
        if (count_to_drop <= 0) {
          full_list;
        } else {
          drop(count_to_drop - 1, list_dropping_one);
        }
      }
    }
  };

let rec take: (int, list('a)) => list('a) =
  (count_to_take, starting_list) => {
    if (count_to_take < 0) {
      [];
    } else {
      switch (starting_list) {
        | [] => []
        | [next_item, ...rest_of_list] => {
            if (count_to_take == 0) {
              [];
            } else {
              [next_item, ...take(count_to_take - 1, rest_of_list)];
            }
          }
        }
    }
  };

let slice: (int, int, list('a)) => list('a) =
  (start_idx, end_idx, starting_list) => {
    if (end_idx < 0) {
      []
    } else {
      take(end_idx - start_idx + 1, drop(start_idx, starting_list));
    }
  }

let rec some: ('a => bool, list('a)) => bool =
  (test_func, list_to_test) => {
    switch (list_to_test) {
    | [] => false;
    | [next_item, ...rest_of_list] => {
        if (test_func(next_item)) {
          true;
        } else {
          some(test_func, rest_of_list);
        }
      }
    };
  };

let chunk: (int, list('a)) => list(list('a)) =
  (chunk_size, starting_list) => {
    if (chunk_size < 1) {
      raise(Invalid("chunk_size must be a positive integer!"));
    } else {
      let evenly_split_remainder = List.length(starting_list) mod chunk_size;
      let last_chunk_length = (evenly_split_remainder === 0) ? chunk_size : evenly_split_remainder;
  
      let (_, _, chunked_list) = starting_list
      |> List.rev
      |> List.fold_left(((current_chunk_count, current_list_length, current_chunked_list), next_item) => {
        let chunk_limit = (current_list_length <= 1) ? last_chunk_length : chunk_size;
        let next_current_chunk_count = (current_chunk_count + 1 === chunk_limit) ? 0 : current_chunk_count + 1;
        if (current_chunk_count === 0) {
          (next_current_chunk_count, current_list_length + 1, [[next_item], ...current_chunked_list])
        } else {
          switch (current_chunked_list) {
          | [first_sublist, ...other_sublists] =>
            (next_current_chunk_count, current_list_length, [[next_item, ...first_sublist], ...other_sublists])
          | [] => raise(Impossible("The first item will always start with a chunk_count of 0!"));
          };
        }
      }, (0, 0, []));
      chunked_list;
    }
  };

let differenceBy: ('a => 'b, list('a), list('a)) => list('a) =
  (comparison_func, comparison_list, starting_list) => {
    List.filter(item => {
      !(comparison_list |> some(comparison_item => comparison_func(comparison_item) == comparison_func(item)))
    }, starting_list);
  };

let difference: (list('a), list('a)) => list('a) =
  (comparison_list, starting_list) =>
    starting_list |> differenceBy(item => item, comparison_list);
