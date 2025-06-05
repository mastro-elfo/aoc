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
  const final = content.split(", ").map(parse).reduce(move, {
    x: 0,
    y: 0,
    direction: "N",
  });
  console.log(Math.abs(final.x) + Math.abs(final.y));
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

function walk(position: Position, instruction: Instruction): Position {
  if (position.direction === "N")
    return { ...position, y: position.y + instruction.blocks };
  if (position.direction === "S")
    return { ...position, y: position.y - instruction.blocks };
  if (position.direction === "E")
    return { ...position, x: position.x + instruction.blocks };
  if (position.direction === "W")
    return { ...position, x: position.x - instruction.blocks };
  return position;
}

function move(position: Position, instruction: Instruction): Position {
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
