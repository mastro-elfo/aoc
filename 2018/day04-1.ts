type Timeshift = {
  id: number;
  start: Date;
  end: Date;
};

function solution(content: string) {
  const timeshifts = parse(content.split("\n").toSorted());
  const guards = groupByGuard(timeshifts);
  const worstGuard = mostAsleepGuard(guards);
  const minutes = groupByMinute(
    timeshifts.filter(({ id }) => id === worstGuard)
  );
  const worstMinute = mostAsleepMinute(minutes);
  return worstGuard * worstMinute;
}

function mostAsleepMinute(minutes: Record<number, number>) {
  return Number(
    Object.entries(minutes).toSorted(
      ([_aKey, aVal], [_bKey, bVal]) => bVal - aVal
    )[0][0]
  );
}

function groupByMinute(timeshifts: Timeshift[]) {
  const minutes: Record<number, number> = {};
  timeshifts.forEach(({ start, end }) => {
    const current = new Date(start);
    while (current < end) {
      minutes[current.getMinutes()] = (minutes[current.getMinutes()] ?? 0) + 1;
      current.setMinutes(current.getMinutes() + 1);
    }
  });
  return minutes;
}

function mostAsleepGuard(guards: Record<number, number>) {
  return Number(
    Object.entries(guards).toSorted(
      ([_aKey, aVal], [_bKey, bVal]) => bVal - aVal
    )[0][0]
  );
}

function groupByGuard(timeshifts: Timeshift[]) {
  const guards: Record<number, number> = {};
  timeshifts.forEach(({ id, start, end }) => {
    guards[id] = (guards[id] ?? 0) + difference(start, end);
  });
  return guards;
}

function difference(start: Date, end: Date) {
  return (+end - +start) / 60_000;
}

function parse(lines: string[]): Timeshift[] {
  let guardId: number | null = null;
  const output: Timeshift[] = [];
  let start: Date | null = null;
  lines.forEach((line) => {
    const matches = line.match(/\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2})\] (.+)/);
    if (matches === null) throw new Error(`Invalid line: ${line}`);
    const date = matches.at(1);
    const note = matches.at(2);
    if (!date || !note) throw new Error(`Invalid line: ${line}`);

    const begins = note.match(/Guard #(\d+) begins shift/);
    const asleep = note.includes("falls asleep");
    const wakeup = note.includes("wakes up");

    if (begins !== null) {
      guardId = Number(begins.at(1));
    } else if (asleep) {
      start = new Date(date);
    } else if (wakeup && guardId !== null && start !== null) {
      output.push({
        id: guardId,
        start,
        end: new Date(date),
      });
    }
  });

  return output;
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
