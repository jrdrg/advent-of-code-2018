module TupleMap =
  Map.Make(
    {
      type t = (int, int);
      let compare = compare;
    },
  );

type ids = Belt.Set.Int.t;

type fabric = TupleMap.t(ids);

let getWithDefault = (key: (int, int), f: fabric) =>
  switch (f |> TupleMap.find(key)) {
  | v => v
  | exception Not_found => Belt.Set.Int.empty
  };

let parseClaim = (claim: string) => {
  let matches =
    claim
    |> Js.String.match(
         [%re "/#(\\d+)\\s*@\\s*(\\d+),(\\d+)\\:\\s*(\\d+)x(\\d+)/"],
       );
  switch (Belt.Option.getWithDefault(matches, [||])) {
  | [|_, id, px, py, w, h|] => (
      int_of_string(id),
      int_of_string(px),
      int_of_string(py),
      int_of_string(w),
      int_of_string(h),
    )
  | _ => ((-1), 0, 0, 0, 0)
  };
};

let generateKeys = (w, h) => {
  let area = w * h;
  Array.init(
    area,
    i => {
      let x = i / h;
      let y = i mod h;
      (x, y);
    },
  );
};

let addClaim = (claims: fabric, id: int, pos: (int, int), size: (int, int)) => {
  let (px, py) = pos;
  let (w, h) = size;
  let updateClaimAt = (x, y, id, c: fabric) : fabric => {
    let key = (x, y);
    let v = Belt.Set.Int.add(c |> getWithDefault(key), id);
    c |> TupleMap.add(key, v);
  };
  generateKeys(w, h)
  |> Array.fold_left(
       (c, (x, y)) => c |> updateClaimAt(x + px, y + py, id),
       claims,
     );
};

let countOverlapping = (claims: fabric) => {
  let count = ref(0);
  claims
  |> TupleMap.filter((_k, v) => Belt.Set.Int.size(v) > 1)
  |> TupleMap.iter((_k, _v) => count := count^ + 1);
  count^;
};

let claims =
  Input.input
  |> Array.fold_left(
       (c, inp) => {
         let (id, x, y, w, h) = parseClaim(inp);
         addClaim(c, id, (x, y), (w, h));
       },
       TupleMap.empty,
     );

let overlapping = claims |> countOverlapping;

Js.log2("Part 1:", overlapping);

let findNonOverlapping = (claims: fabric) => {
  open Belt;
  let c = ref(Map.Int.empty);
  claims
  |> TupleMap.iter((_k, v) => {
       let idCount = Set.Int.size(v);
       Set.Int.forEach(
         v,
         i => {
           let existing = max(Map.Int.getWithDefault(c^, i, 1), idCount);
           c := Map.Int.set(c^, i, existing);
         },
       );
     });
  let noOverlap = Map.Int.findFirstBy(c^, (_k, v) => v == 1);
  Option.mapWithDefault(noOverlap, -1, ((k, _v)) => k);
};

let nonOverlappingId = claims |> findNonOverlapping;

Js.log2("Part 2:", nonOverlappingId);
