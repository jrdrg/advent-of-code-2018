type sleepStatus =
  | BeginShift(int)
  | Asleep(int)
  | Awake(int);

let parse = line => {
  /* [1518-05-30 00:27] wakes up */
  let matches = line |> Js.String.match([%re "/\\[([\\d-\\s:]+)\\]\\s(.*)/"]);
  let list = Belt.Option.getWithDefault(matches, [||]) |> Array.to_list;
  switch (list) {
  | [_, timestamp, event, ..._] => (timestamp, event)
  | _ => ("", "")
  };
};

let compareByTimestamp = ((timestamp1, _), (timestamp2, _)) =>
  String.compare(timestamp1, timestamp2);

let sortByTimestamp = events => {
  events |> Array.sort(compareByTimestamp);
  events;
};

let getMinute = timestamp => {
  let matches = timestamp |> Js.String.match([%re "/\\d{2}:(\\d{2})/"]);
  switch (matches) {
  | Some([|_, minute|]) => int_of_string(minute)
  | _ => 0
  };
};

let updateGuardSleeping = (guardId, minute, guardsSleeping) =>
  Belt.Map.Int.update(guardsSleeping, guardId, g =>
    switch (g) {
    | Some(minuteMap) =>
      Some(
        Belt.Map.Int.update(minuteMap, minute, m =>
          Some(Belt.Option.getWithDefault(m, 0) + 1)
        ),
      )
    | None => Some(Belt.Map.Int.set(Belt.Map.Int.empty, minute, 1))
    }
  );

let getGuardId = desc => {
  let matches = desc |> Js.String.match([%re "/Guard #(\\d+) begins shift/"]);
  switch (matches) {
  | Some([|_, id|]) => id |> int_of_string
  | _ => (-1)
  };
};

let toEvent = ((timestamp, desc)) =>
  switch (desc) {
  | "wakes up" => Awake(getMinute(timestamp))
  | "falls asleep" => Asleep(getMinute(timestamp))
  | _ => BeginShift(getGuardId(desc))
  };

let buildMap = ((guardId, sleepMinute, guardsSleeping), event) =>
  switch (toEvent(event)) {
  | BeginShift(id) => (id, 0, guardsSleeping)
  | Awake(minute) =>
    let endMinute = sleepMinute > minute ? minute + 60 : minute;
    let updatedMap =
      Belt.Array.range(sleepMinute, endMinute - 1)
      |> Array.fold_left(
           (map, min) => updateGuardSleeping(guardId, min mod 60, map),
           guardsSleeping,
         );
    (guardId, 0, updatedMap);
  | Asleep(minute) => (
      guardId,
      minute,
      guardsSleeping |> updateGuardSleeping(guardId, minute),
    )
  };

let (_, _, sleepMap) =
  Input.input
  |> Array.map(parse)
  |> sortByTimestamp
  |> Array.fold_left(buildMap, (0, 0, Belt.Map.Int.empty));

let (maxGuardId, mostAsleepMinute, _) =
  Belt.Map.Int.(
    reduce(
      sleepMap,
      (0, 0, 0),
      ((id, minute, sumMinutes), guardId, minuteMap) => {
        let (sum, mostAsleepMin, _) =
          reduce(
            minuteMap,
            (0, 0, 0),
            ((sum, minuteOfSum, maxTimesAsleep), min, timesAsleep) =>
            if (timesAsleep > maxTimesAsleep) {
              (sum + timesAsleep, min, timesAsleep);
            } else {
              (sum + timesAsleep, minuteOfSum, maxTimesAsleep);
            }
          );
        if (sum > sumMinutes) {
          (guardId, mostAsleepMin, sum);
        } else {
          (id, minute, sumMinutes);
        };
      },
    )
  );

Js.log4(
  "Part 1:",
  maxGuardId,
  mostAsleepMinute,
  maxGuardId * mostAsleepMinute,
);

let (guardId, mostFrequentMinute, _) =
  Belt.Map.Int.(
    reduce(
      sleepMap,
      (0, 0, 0),
      ((id, minute, highestFrequency), guardId, minuteMap) => {
        let (min, freq) =
          reduce(minuteMap, (0, 0), ((maxMin, maxCount), min, timesAsleep) =>
            if (timesAsleep > maxCount) {
              (min, timesAsleep);
            } else {
              (maxMin, maxCount);
            }
          );
        if (freq > highestFrequency) {
          (guardId, min, freq);
        } else {
          (id, minute, highestFrequency);
        };
      },
    )
  );

Js.log4("Part 2:", guardId, mostFrequentMinute, guardId * mostFrequentMinute);
