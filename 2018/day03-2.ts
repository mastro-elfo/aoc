type Rectangle = {
  id: number;
  left: number;
  top: number;
  right: number;
  bottom: number;
};

function solution(content: string) {
  return content
    .split("\n")
    .map(parse)
    .filter((r1, _, rects) =>
      rects.every((r2) => r1.id === r2.id || !overlap(r1, r2))
    )[0].id;
}

function overlap(r1: Rectangle, r2: Rectangle) {
  return (
    isInside(r1, r2) ||
    isInside(r2, r1) ||
    isOutside(r1, r2) ||
    isOutside(r2, r1)
  );
}

function isOutside(r1: Rectangle, r2: Rectangle) {
  return (
    r2.left <= r1.left &&
    r1.left < r2.right &&
    r1.top <= r2.top &&
    r2.top < r1.bottom
  );
}

function isInside(r1: Rectangle, r2: Rectangle) {
  return (
    isInternal(r1.left, r1.top, r2) ||
    isInternal(r1.left, r1.bottom, r2) ||
    isInternal(r1.right, r1.top, r2) ||
    isInternal(r1.right, r1.bottom, r2)
  );
}

function isInternal(left: number, top: number, rect: Rectangle) {
  return (
    rect.left <= left &&
    left < rect.right &&
    rect.top <= top &&
    top < rect.bottom
  );
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
