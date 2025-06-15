type Position = [number, number];

function solution(content: string) {
  const lines = content.split("\n");
  let current: Position = [1, 1];
  let code = "";
  lines.forEach((line) => {
    line.split("").forEach((char) => {
      current = follow(char, current, 2);
    });
    code += toCode(current);
  });
  return code;
}

function toCode(position: Position) {
  if (position[0] === 0 && position[1] === 0) return "1";
  if (position[0] === 1 && position[1] === 0) return "2";
  if (position[0] === 2 && position[1] === 0) return "3";
  if (position[0] === 0 && position[1] === 1) return "4";
  if (position[0] === 1 && position[1] === 1) return "5";
  if (position[0] === 2 && position[1] === 1) return "6";
  if (position[0] === 0 && position[1] === 2) return "7";
  if (position[0] === 1 && position[1] === 2) return "8";
  if (position[0] === 2 && position[1] === 2) return "9";
  return "";
}

function follow(
  instruction: string,
  current: Position,
  size: number
): Position {
  if (instruction === "U") {
    return [current[0], Math.max(0, current[1] - 1)];
  }
  if (instruction === "D") {
    return [current[0], Math.min(size, current[1] + 1)];
  }
  if (instruction === "L") {
    return [Math.max(0, current[0] - 1), current[1]];
  }
  if (instruction === "R") {
    return [Math.min(size, current[0] + 1), current[1]];
  }
  return current;
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
