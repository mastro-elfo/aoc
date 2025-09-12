type Instruction = {
  qty: number;
  from: number;
  to: number;
};

type Stacks = string[][];

function solution(content: string) {
  const [stacks, instructions] = parse(content);

  return tops(instructions.reduce((acc, cur) => act(cur, acc), stacks));
}

function tops(stacks: Stacks) {
  return stacks.map((stack) => stack[0]).join("");
}

function act(instruction: Instruction, stacks: Stacks): Stacks {
  const { qty, from, to } = instruction;
  const copy = stacks.slice();

  const toMove = copy[from].splice(0, qty).toReversed();
  copy[to] = [...toMove, ...copy[to]];

  return copy;
}

function parse(content: string): [Stacks, Instruction[]] {
  const [part1, part2] = content.split("\n\n");
  return [parseStacks(part1), parseInstructions(part2)];
}

function parseStacks(content: string): Stacks {
  const lines = content.split("\n");
  const size = (lines[0].length + 1) / 4;
  const stacks: Stacks = Array.from({ length: size }).map(() => []);
  lines.slice(0, -1).forEach((line) => {
    Array.from({ length: size })
      .map((_, idx) => idx * 4 + 1)
      .forEach((index, idx) => {
        const crate = line[index].trim();
        if (crate) {
          stacks[idx].push(crate);
        }
      });
  });
  return stacks;
}

function parseInstructions(content: string) {
  return content
    .split("\n")
    .filter((line) => line)
    .map(parseProcedure);
}

function parseProcedure(line: string): Instruction {
  const match = line.match(/move (\d+) from (\d+) to (\d+)/);
  if (match === null) throw new Error(`Invalid line: ${line}`);
  return {
    qty: Number(match[1]),
    from: Number(match[2]) - 1,
    to: Number(match[3]) - 1,
  };
}

Deno.readTextFile("day05.dat")
  .then((content) => content)
  .then(solution)
  .then(console.log);
