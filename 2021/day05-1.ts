type Coords = {
  x: number;
  y: number;
};

type Vector = {
  start: Coords;
  end: Coords;
};

function solution(content: string) {
  const straight = content.split("\n").map(parse).filter(isStraight);
  const { x: mx, y: my } = getLimit(straight);
  return each(mx, my).filter((cc) => findTwo(cc, straight)).length;
}

function findOne(coords: Coords, vecs: Vector[]) {
  return !!vecs.find((v) => isInLine(coords, v));
}

function findTwo(coords: Coords, vecs: Vector[]) {
  const index = vecs.findIndex((v) => isInLine(coords, v));
  if (index === -1) return false;
  return findOne(coords, vecs.slice(index + 1));
}

function isInLine(
  { x, y }: Coords,
  { start: { x: sx, y: sy }, end: { x: ex, y: ey } }: Vector
) {
  if (allEqual(x, sx, ex)) return isBetween(y, sy, ey);
  if (allEqual(y, sy, ey)) return isBetween(x, sx, ex);
  return false;
}

function allEqual(x: number, y: number, z: number) {
  return x === y && y === z;
}

function isBetween(x: number, left: number, right: number) {
  return (left <= x && x <= right) || (right <= x && x <= left);
}

function each(mx: number, my: number) {
  return Array.from({ length: mx + 1 })
    .map((_, x) => Array.from({ length: my + 1 }).map((_, y) => ({ x, y })))
    .flat();
}

function getLimit(vecs: Vector[]): Coords {
  return vecs.reduce(
    (acc, cur) => ({
      x: Math.max(acc.x, cur.start.x, cur.end.x),
      y: Math.max(acc.y, cur.start.y, cur.end.y),
    }),
    { x: 0, y: 0 } as Coords
  );
}

function isStraight({
  start: { x: sx, y: sy },
  end: { x: ex, y: ey },
}: Vector) {
  return sx === ex || sy === ey;
}

function parse(line: string): Vector {
  const match = line.match(/(\d+),(\d+) -> (\d+),(\d+)/);
  if (match === null) throw new Error(`Invalid line ${line}`);
  return {
    start: { x: Number(match[1]), y: Number(match[2]) },
    end: { x: Number(match[3]), y: Number(match[4]) },
  };
}

Deno.readTextFile("day05.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
