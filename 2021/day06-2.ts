type FishGroup = {
  day: number;
  size: number;
};

function solution(content: string) {
  return Array.from({ length: 256 })
    .reduce((acc: FishGroup[]) => play(acc), parse(content))
    .reduce((acc, { size }) => acc + size, 0);
}

function play(groups: FishGroup[]): FishGroup[] {
  return clean(nextDay(generate(groups)));
}

function clean(groups: FishGroup[]): FishGroup[] {
  return groups
    .filter(({ day }) => day !== 6)
    .concat([
      {
        day: 6,
        size: groups
          .filter(({ day }) => day === 6)
          .map(({ size }) => size)
          .reduce((acc, cur) => acc + cur, 0),
      },
    ])
    .filter(({ size }) => size != 0);
}

function nextDay(groups: FishGroup[]): FishGroup[] {
  return groups.map((g) => ({
    day: g.day === 0 ? 6 : g.day - 1,
    size: g.size,
  }));
}

function generate(groups: FishGroup[]): FishGroup[] {
  return groups.concat(
    groups.filter(({ day }) => day === 0).map(({ size }) => ({ day: 9, size }))
  );
}

function parse(content: string): FishGroup[] {
  const data = content.split(",").map(Number);
  return Array.from(new Set(data)).map((day) => ({
    day,
    size: data.filter((d) => d === day).length,
  }));
}

Deno.readTextFile("day06.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
