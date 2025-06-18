function solution(content: string) {
  const program = content.split(",").map((item) => parseInt(item));
  for (let noun = 0; noun < 100; noun++) {
    for (let verb = 0; verb < 100; verb++) {
      const copy = program.slice();
      copy[1] = noun;
      copy[2] = verb;
      const result = execute(copy);
      if (result === 19690720) {
        return 100 * noun + verb;
      }
    }
  }
}

function execute(program: number[]) {
  let copy = program.slice();
  let halt = false;
  let current = 0;
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
