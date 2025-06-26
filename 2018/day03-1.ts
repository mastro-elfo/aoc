type Rectangle = {
  id: number;
  left: number;
  top: number;
  right: number;
  bottom: number;
};

type Fabric = Record<string, number>;

function solution(content: string) {
  const fabric: Fabric = {};
  content
    .split("\n")
    .map(parse)
    .forEach((rect) => {
      draw(rect, fabric);
    });
  return Object.values(fabric).filter((value) => value === 0).length;
}

function draw(rect: Rectangle, fabric: Fabric) {
  Array.from({ length: rect.right - rect.left }).forEach((_, xIndex) => {
    Array.from({ length: rect.bottom - rect.top }).forEach((_, yIndex) => {
      draw_at(rect.id, rect.left + xIndex, rect.top + yIndex, fabric);
    });
  });
}

function draw_at(rid: number, left: number, top: number, fabric: Fabric) {
  const index = hash(left, top);
  if (fabric[index] === undefined) {
    fabric[index] = rid;
  } else {
    fabric[index] = 0;
  }
}

function hash(left: number, top: number) {
  return `${left}-${top}`;
}

function parse(line: string): Rectangle {
  const match = line.match(/#(\d+)\s*@\s*(\d+),(\d+):\s*(\d+)x(\d+)/);
  if (match === null) {
    throw new Error(`Invalid line: ${line}`);
  }
  const left = Number(match[2]);
  const top = Number(match[3]);
  return {
    id: Number(match[1]),
    left,
    top,
    right: left + Number(match[4]),
    bottom: top + Number(match[5]),
  };
}

Deno.readTextFile("day03.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
