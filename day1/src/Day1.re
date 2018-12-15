let updateFrequency = (frequency: int, change: string) => {
  let firstChar = change.[0];
  let rest = String.sub(change, 1, String.length(change) - 1);
  switch (firstChar, rest) {
  | ('+', value) => frequency + int_of_string(value)
  | ('-', value) => frequency - int_of_string(value)
  | _ => frequency
  };
};

/* Part 1 */
let output = Input.input |> Array.fold_left(updateFrequency, 0);

Js.log2("Part 1", output);

/* Part 2 */
type intMap = Belt.Map.Int.t(int);

let rec checkFrequencyCounts =
        (input: array(string), countMap: intMap, frequency: int, index: int) => {
  let idx = index mod Array.length(input);
  let change = input[idx];
  let freq = updateFrequency(frequency, change);
  let timesCounted = Belt.Map.Int.getWithDefault(countMap, freq, 0) + 1;
  switch (timesCounted) {
  | 2 => freq
  | _ =>
    checkFrequencyCounts(
      input,
      Belt.Map.Int.set(countMap, freq, timesCounted),
      freq,
      idx + 1,
    )
  };
};

let frequencyCounts: intMap = Belt.Map.Int.empty;

let firstTwice = checkFrequencyCounts(Input.input, frequencyCounts, 0, 0);

Js.log2("Part 2", firstTwice);
