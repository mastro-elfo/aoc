type Position = [number, number];

function solution(content: string) {
  const lines = content.split("\n");
  let current: Position = [0, 2];
  let code = "";
  lines.forEach((line) => {
    line.split("").forEach((char) => {
      current = follow(char, current);
    });
    code += toCode(current);
  });
  return code;
}

function toCode(position: Position) {
  if (position[0] === 2 && position[1] === 0) return "1";
  if (position[0] === 1 && position[1] === 1) return "2";
  if (position[0] === 2 && position[1] === 1) return "3";
  if (position[0] === 3 && position[1] === 1) return "4";
  if (position[0] === 0 && position[1] === 2) return "5";
  if (position[0] === 1 && position[1] === 2) return "6";
  if (position[0] === 2 && position[1] === 2) return "7";
  if (position[0] === 3 && position[1] === 2) return "8";
  if (position[0] === 4 && position[1] === 2) return "9";
  if (position[0] === 1 && position[1] === 3) return "A";
  if (position[0] === 2 && position[1] === 3) return "B";
  if (position[0] === 3 && position[1] === 3) return "C";
  if (position[0] === 2 && position[1] === 4) return "D";
  return "";
}

function follow(instruction: string, current: Position): Position {
  if (isAllowed("U", instruction, current)) {
    return [current[0], current[1] - 1];
  }
  if (isAllowed("D", instruction, current)) {
    return [current[0], current[1] + 1];
  }
  if (isAllowed("L", instruction, current)) {
    return [current[0] - 1, current[1]];
  }
  if (isAllowed("R", instruction, current)) {
    return [current[0] + 1, current[1]];
  }
  return current;
}

function isAllowed(direction: string, instruction: string, current: Position) {
  if (instruction !== direction) return false;
  if (direction === "U")
    return !["0-2", "1-1", "2-0", "3-1", "4-2"].includes(hash(current));
  if (direction === "D")
    return !["0-2", "1-3", "2-4", "3-3", "4-2"].includes(hash(current));
  if (direction === "L")
    return !["2-0", "1-1", "0-2", "1-3", "2-4"].includes(hash(current));
  if (direction === "R")
    return !["2-0", "3-1", "4-2", "3-3", "2-4"].includes(hash(current));
  return false;
}

function hash(position: Position) {
  return `${position[0]}-${position[1]}`;
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
