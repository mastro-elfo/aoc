function solution(content: string) {
  return execute(content.split(",").map((item) => parseInt(item)));
}

function execute(program: number[]) {
  let copy = program.slice();
  let halt = false;
  let current = 0;
  copy[1] = 12;
  copy[2] = 2;
  while (!halt) {
    const result = executeOne(current, copy);
    current = result[0];
    copy = result[1];
    halt = result[2];
  }
  return copy[0];
}

function executeOne(
  current: number,
  program: number[]
): [number, number[], boolean] {
  const opcode = program[current];
  const copy = program.slice();
  if (opcode == 1) {
    copy[copy[current + 3]] = copy[copy[current + 1]] + copy[copy[current + 2]];
    return [current + 4, copy, false];
  }
  if (opcode == 2) {
    copy[copy[current + 3]] = copy[copy[current + 1]] * copy[copy[current + 2]];
    return [current + 4, copy, false];
  }
  if (opcode == 99) {
    return [0, copy, true];
  }
  throw Error(
    `Invalid opcode ${opcode} at ${current} in ${program.slice(
      current,
      current + 4
    )}`
  );
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
