type Coord = {
  x: number;
  y: number;
};
type Direction = "^" | "v" | "<" | ">";
type Guard = Coord & { direction: Direction };

function solution(content: string) {
  const rows = content.split("\n").length;
  let current = parseStartPosition(rows, content);
  const obstacles = parseObstacles(rows, content);
  const limits = parseLimits(rows, content);
  const positions: Guard[] = [current];

  while (true) {
    const newCurrent = move(current);
    if (isOutside(limits, newCurrent)) {
      break;
    } else if (isObstacle(obstacles, newCurrent)) {
      current = rotate(current);
    } else {
      current = newCurrent;
      positions.push(current);
    }
  }

  return new Set(positions.map((item) => `${item.x}-${item.y}`)).size;
}

function rotate(current: Guard): Guard {
  const { x, y, direction } = current;
  if (direction === "^") {
    return { x, y, direction: ">" };
  }
  if (direction === "v") {
    return { x, y, direction: "<" };
  }
  if (direction === "<") {
    return { x, y, direction: "^" };
  }
  if (direction === ">") {
    return { x, y, direction: "v" };
  }
  throw new Error(`Invalid direction: ${direction}`);
}

function move(current: Guard): Guard {
  const { x, y, direction } = current;
  if (direction === "^") {
    return { x, y: y - 1, direction };
  }
  if (direction === "v") {
    return { x, y: y + 1, direction };
  }
  if (direction === "<") {
    return { x: x - 1, y, direction };
  }
  if (direction === ">") {
    return { x: x + 1, y, direction };
  }
  throw new Error(`Invalid direction: ${direction}`);
}

function isObstacle(obstacles: Coord[], current: Guard) {
  const { x, y } = current;
  return !!obstacles.find((item) => item.x === x && item.y === y);
}

function isOutside(limits: Coord, current: Guard) {
  const { x: right, y: bottom } = limits;
  const { x, y } = current;
  return x < 0 || y < 0 || x > right || y > bottom;
}

function parseLimits(rows: number, content: string): Coord {
  return indexToCoord(
    rows,
    content.split("").filter((item) => item !== "\n").length - 1
  );
}

function parseObstacles(rows: number, content: string): Coord[] {
  return content
    .split("")
    .filter((item) => item !== "\n")
    .map((item, index) => {
      if (item === "#") return indexToCoord(rows, index);
      return null;
    })
    .filter((item) => item !== null);
}

function parseStartPosition(rows: number, content: string): Guard {
  const direction = content
    .split("")
    .find((item) => "^v<>".includes(item)) as Direction;
  const index = content
    .split("")
    .filter((item) => item !== "\n")
    .findIndex((item) => "^v<>".includes(item));
  const { x, y } = indexToCoord(rows, index);
  return {
    x,
    y,
    direction,
  };
}

function indexToCoord(rows: number, index: number): Coord {
  return {
    x: index % rows,
    y: Math.floor(index / rows),
  };
}

Deno.readTextFile("day06.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
