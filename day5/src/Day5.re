let isLowerCase = c => Char.lowercase(c) == c;

let isUpperCase = c => Char.uppercase(c) == c;

let isOppositeCase = (c1, c2) =>
  Char.lowercase(c1) == Char.lowercase(c2)
  && (
    isLowerCase(c1)
    && isUpperCase(c2)
    || isUpperCase(c1)
    && isLowerCase(c2)
  );

let rec processChar = (str, index, excludeChar, currStack) =>
  if (index >= String.length(str)) {
    currStack;
  } else {
    let current = str.[index];
    switch (excludeChar) {
    | Some(exclude) when Char.lowercase(current) == Char.lowercase(exclude) =>
      ()
    | _ =>
      switch (Stack.pop(currStack)) {
      | ch =>
        if (! isOppositeCase(current, ch)) {
          currStack |> Stack.push(ch);
          currStack |> Stack.push(current);
        }
      | exception Stack.Empty => currStack |> Stack.push(current)
      }
    };
    processChar(str, index + 1, excludeChar, currStack);
  };

let stack = Stack.create() |> processChar(Input.input, 0, None);

Js.log2("Part 1:", Stack.length(stack));

let shortest =
  Belt.Array.range(int_of_char('a'), int_of_char('z'))
  |> Array.fold_left(
       (currentMin, code) => {
         let ch = char_of_int(code);
         let length =
           Stack.create()
           |> processChar(Input.input, 0, Some(ch))
           |> Stack.length;
         length < currentMin ? length : currentMin;
       },
       String.length(Input.input),
     );

Js.log2("Part 2:", shortest);
