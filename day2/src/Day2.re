module IntMap = Belt.Map.Int;

let rec letterCounts = (str: string, index: int, counts: IntMap.t(int)) =>
  switch (index) {
  | x when x == String.length(str) => counts
  | _ =>
    let letter = str.[index] |> int_of_char;
    let count = IntMap.getWithDefault(counts, letter, 0);
    let incrementedCount = IntMap.set(counts, letter, count + 1);
    letterCounts(str, index + 1, incrementedCount);
  };

let b2i = b => b ? 1 : 0;

let hasExactly2 = (counts: IntMap.t(int)) =>
  IntMap.some(counts, (_k, v) => v == 2) |> b2i;

let hasExactly3 = (counts: IntMap.t(int)) =>
  IntMap.some(counts, (_k, v) => v === 3) |> b2i;

let sums =
  Input.input
  |> Array.map(id => {
       let counts = letterCounts(id, 0, IntMap.empty);
       (hasExactly2(counts), hasExactly3(counts));
     })
  |> Array.fold_left(
       ((count2, count3), (has2, has3)) => (count2 + has2, count3 + has3),
       (0, 0),
     );

let (sum2, sum3) = sums;

Js.log2("Part 1", sum2 * sum3);

/* Part 2 */
module TupleMap =
  Map.Make(
    {
      type t = (string, int);
      let compare = compare;
    },
  );

type substrMap = TupleMap.t(string);

let rec matchSubstrings = (str: string, index: int, dict: substrMap) => {
  let first = s => String.sub(s, 0, index);
  let last = s => String.sub(s, index + 1, String.length(s) - 1 - index);
  switch (index) {
  | x when x == String.length(str) => (false, dict, ("", (-1)))
  | _ =>
    let substr =
      switch (index) {
      | 0 => last(str)
      | x when x == String.length(str) - 1 => first(str)
      | _ =>
        let f = first(str);
        let l = last(str);
        f ++ l;
      };
    let key = (substr, index);
    switch (dict |> TupleMap.find(key)) {
    | _ => (true, dict, key)
    | exception Not_found =>
      matchSubstrings(str, index + 1, TupleMap.add(key, str, dict))
    };
  };
};

let rec findPrototype = (strings: array(string), index: int, dict: substrMap) =>
  switch (index) {
  | x when x == Array.length(strings) => "Nothing found"
  | _ =>
    let str = strings[index];
    let (match, updatedDict, key) = matchSubstrings(str, 0, dict);
    if (match) {
      let (substr, _) = key;
      substr;
    } else {
      findPrototype(strings, index + 1, updatedDict);
    };
  };

let commonLetters = findPrototype(Input.input, 0, TupleMap.empty);

Js.log2("Part 2", commonLetters);
