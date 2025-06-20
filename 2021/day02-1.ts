type Position = {
  forward: number;
  depth: number;
};

type Instruction = {
  direction: string;
  amount: number;
};

function solution(content: string) {
  const position = content
    .split("\n")
    .map((item) => parse(item))
    .reduce((acc, cur) => move(acc, cur), { depth: 0, forward: 0 });
  return position.depth * position.forward;
}

function move(position: Position, instruction: Instruction): Position {
  const { amount, direction } = instruction;
  const { depth, forward } = position;
  if (direction === "forward") {
    return { depth, forward: forward + amount };
  }
  if (direction === "up") {
    return { depth: depth - amount, forward };
  }
  if (direction === "down") {
    return { depth: depth + amount, forward };
  }
  return position;
}

function parse(line: string): Instruction {
  const split = line.split(" ");
  return { amount: Number(split[1]), direction: split[0] };
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
