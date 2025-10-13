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
        nearest(pivots, { x, y })
      )
    )
    .flat();
  const borders = borderlines(map, vlimit);
  const noBord = map.filter((c) => !!c && !includes(c, borders));
  return Math.max(
    ...Array.from(new Set(noBord)).map(
      (f) => noBord.filter((c) => c?.x === f?.x && c?.y === f?.y).length
    )
  );
}

function includes(c: Coords, coords: Set<Coords>) {
  return !!Array.from(coords).find((i) => i.x === c.x && i.y === c.y);
}

function borderlines(coords: (Coords | null)[], vlimit: number) {
  return new Set(
    coords
      .slice(0, vlimit + 1)
      .concat(coords.slice(-vlimit - 1))
      .concat(coords.filter((_, index) => index % (vlimit + 1) === 0))
      .concat(
        coords.slice(vlimit).filter((_, index) => index % (vlimit + 1) === 0)
      )
      .filter((c) => !!c)
  );
}

function nearest(pivots: Coords[], c: Coords) {
  const distances = pivots.map((p) => manhattan(p, c));
  const minDist = Math.min(...distances);
  if (distances.filter((item) => item === minDist).length > 1) {
    return null;
  }
  return pivots[distances.indexOf(minDist)];
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
