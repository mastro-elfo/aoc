type Coords = {
  x: number;
  y: number;
};

function solution(content: string) {
  const pivots = content.split("\n").map(parse);
  const hlimit = Math.max(...pivots.map(({ x }) => x));
  const vlimit = Math.max(...pivots.map(({ y }) => y));
  const map = Array.from({ length: hlimit + 1 })
    .map((_, x) =>
      Array.from({ length: vlimit + 1 }).map((_, y) =>
        pivots.reduce((acc, cur) => acc + manhattan({ x, y }, cur), 0)
      )
    )
    .flat();
  return map.filter((item) => item < 10_000).length;
}

function manhattan(c1: Coords, c2: Coords) {
  return Math.abs(c1.x - c2.x) + Math.abs(c1.y - c2.y);
}

function parse(line: string): Coords {
  const [x, y] = line.split(", ").map(Number);
  return { x, y };
}

Deno.readTextFile("day06.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
