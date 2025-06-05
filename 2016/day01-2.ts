type Direction = "N" | "S" | "W" | "E";
type Turn = "L" | "R";
type Instruction = {
  turn: Turn;
  blocks: number;
};
type Position = {
  x: number;
  y: number;
  direction: Direction;
};

function solution(content: string) {
  const instructions = content.split(", ").map(parse);

  let current: Position = { x: 0, y: 0, direction: "N" };
  let visited: Position[] = [];
  instructions.some((instruction) => {
    const newVisited = move(current, instruction);
    current = newVisited.at(-1)!;
    const visitedTwice = alreadyVisited(visited, newVisited);
    if (visitedTwice) {
      console.log(Math.abs(visitedTwice.x) + Math.abs(visitedTwice.y));
      return true;
    }
    visited = visited.concat(newVisited);
  });
}

function alreadyVisited(visited: Position[], newVisited: Position[]) {
  return visited.find((item) =>
    newVisited.find((itm) => itm.x === item.x && itm.y === item.y)
  );
}

function turn(position: Position, instruction: Instruction): Position {
  if (position.direction === "N")
    return { ...position, direction: instruction.turn === "L" ? "W" : "E" };
  if (position.direction === "S")
    return { ...position, direction: instruction.turn === "L" ? "E" : "W" };
  if (position.direction === "E")
    return { ...position, direction: instruction.turn === "L" ? "N" : "S" };
  if (position.direction === "W")
    return { ...position, direction: instruction.turn === "L" ? "S" : "N" };
  return position;
}

function walk(position: Position, instruction: Instruction): Position[] {
  if (position.direction === "N")
    return Array.from({ length: instruction.blocks }).map((_, index) => ({
      ...position,
      y: position.y + index + 1,
    }));
  if (position.direction === "S")
    return Array.from({ length: instruction.blocks }).map((_, index) => ({
      ...position,
      y: position.y - index - 1,
    }));
  if (position.direction === "E")
    return Array.from({ length: instruction.blocks }).map((_, index) => ({
      ...position,
      x: position.x + index + 1,
    }));
  if (position.direction === "W")
    return Array.from({ length: instruction.blocks }).map((_, index) => ({
      ...position,
      x: position.x - index - 1,
    }));
  return [];
}

function move(position: Position, instruction: Instruction): Position[] {
  return walk(turn(position, instruction), instruction);
}

function parse(item: string): Instruction {
  return {
    turn: item[0] === "L" ? "L" : "R",
    blocks: parseInt(item.substring(1)),
  };
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution);
