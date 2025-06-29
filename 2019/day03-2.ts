type Coords = {
  x: number;
  y: number;
};
type Instruction = {
  direction: string;
  amount: number;
};
type Segment = {
  start: Coords;
  end: Coords;
};

function solution(content: string) {
  const lines = content.split("\n");
  const seg1 = parse(lines[0]);
  const seg2 = parse(lines[1]);
  const wire1 = untangle(seg1);
  const wire2 = untangle(seg2);
  return seg1
    .map((s1) => seg2.filter((s2) => intersect(s1, s2)).map((s2) => [s1, s2]))
    .flat()
    .map(([s1, s2]) =>
      unfold(s1).find((coords) =>
        (coords.x !== 0 || coords.y !== 0) &&
        unfold(s2).find((item) => item.x === coords.x && item.y === coords.y)
          ? coords
          : null
      )
    )
    .filter((item) => item)
    .map((coords) => length(coords!, wire1) + length(coords!, wire2))
    .toSorted()
    .at(0);
}

function length(coords: Coords, wire: Coords[]) {
  return wire.findIndex(
    (value) => value.x === coords.x && value.y === coords.y
  );
}

function untangle(wire: Segment[]): Coords[] {
  return wire.map((segment) => unfold(segment)).flat();
}

function unfold(segment: Segment): Coords[] {
  const {
    start: { x: startX, y: startY },
    end: { x: endX, y: endY },
  } = segment;
  if (isVertical(segment)) {
    const sign = Math.sign(endY - startY);
    return Array.from({ length: Math.abs(startY - endY) }).map((_, index) => ({
      x: startX,
      y: startY + index * sign,
    }));
  }
  const sign = Math.sign(endX - startX);
  return Array.from({ length: Math.abs(startX - endX) }).map((_, index) => ({
    x: startX + index * sign,
    y: startY,
  }));
}

function intersect(s1: Segment, s2: Segment) {
  const {
    start: { x: start1x, y: start1y },
    end: { x: end1x, y: end1y },
  } = s1;
  const {
    start: { x: start2x, y: start2y },
    end: { x: end2x, y: end2y },
  } = s2;
  if (isVertical(s1) && isVertical(s2))
    return (
      start1y === start2y &&
      (between(start1y, start2y, end2y) ||
        between(end1y, start2y, end2y) ||
        between(start2y, start1y, end1y) ||
        between(end2y, start1y, end1y))
    );
  if (!isVertical(s1) && isVertical(s2))
    return between(start1y, start2y, end2y) && between(start2x, start1x, end1x);
  if (isVertical(s1) && !isVertical(s2))
    return between(start1x, start2x, end2x) && between(start2y, start1y, end1y);
  return (
    start1x === start2x &&
    (between(start1x, start2x, end2x) ||
      between(end1x, start2x, end2x) ||
      between(start2x, start1x, end1x) ||
      between(end2x, start1x, end1x))
  );
}

function isVertical({ start: { x: startX }, end: { x: endX } }: Segment) {
  return startX === endX;
}

function between(x: number, start: number, end: number) {
  return start < end ? start <= x && x <= end : end <= x && x <= end;
}

function parse(line: string): Segment[] {
  return line
    .split(",")
    .map((item) => parseInstruction(item))
    .reduce((acc, instr) => {
      const last = acc.at(-1);
      if (last) {
        return [
          ...acc,
          { start: last.end, end: move(last.end, instr) },
        ] as Segment[];
      }
      return [{ start: { x: 0, y: 0 }, end: move({ x: 0, y: 0 }, instr) }];
    }, [] as Segment[]);
}

function move({ x, y }: Coords, { direction, amount }: Instruction): Coords {
  if (direction === "R") return { x: x + amount, y };
  if (direction === "L") return { x: x - amount, y };
  if (direction === "U") return { x, y: y + amount };
  if (direction === "D") return { x, y: y - amount };
  return { x, y };
}

function parseInstruction(word: string): Instruction {
  return {
    amount: Number(word.slice(1)),
    direction: word[0],
  };
}
Deno.readTextFile("day03.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
