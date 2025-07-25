type Timeshift = {
  id: string;
  start: Date;
  end: Date;
};

function solution(content: string) {
  const [key, [val]] = mostAsleepGuard(
    mostAsleepMinute(
      groupByGuardAndMinute(parse(content.split("\n").toSorted()))
    )
  );
  return Number(key) * Number(val);
}

function mostAsleepGuard(guards: Record<string, [string, number]>) {
  return Object.entries(guards).toSorted(
    ([_aKey, [_aMin, aVal]], [_bKey, [_bMin, bVal]]) => bVal - aVal
  )[0];
}

function mostAsleepMinute(guards: Record<string, Record<number, number>>) {
  return Object.fromEntries(
    Object.entries(guards).map(([id, guard]) => [
      id,
      Object.entries(guard).toSorted(
        ([_aId, aVal], [_bId, bVal]) => bVal - aVal
      )[0],
    ])
  );
}

function groupByGuardAndMinute(timeshifts: Timeshift[]) {
  const output: Record<string, Record<number, number>> = {};

  timeshifts.forEach(({ id, end, start }) => {
    if (output[id] === undefined) {
      output[id] = {};
    }
    const current = new Date(start);
    while (current < end) {
      const minute = current.getMinutes();
      output[id][minute] = (output[id][minute] ?? 0) + 1;
      current.setMinutes(minute + 1);
    }
  });

  return output;
}

function parse(lines: string[]): Timeshift[] {
  let guardId: string | null = null;
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
      guardId = begins.at(1) ?? "";
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
