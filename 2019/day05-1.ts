type Output = {
  index: number;
  program: number[];
  halt: boolean;
  output: number | null;
};

function solution(content: string) {
  return execute(content.split(",").map(Number));
}

function execute(program: number[]) {
  let copy = program.slice();
  let halt = false;
  let current = 0;
  let output = null;
  while (!halt) {
    const result = executeOne(current, copy);
    current = result.index;
    copy = result.program;
    halt = result.halt;
    output = result.output ?? output;
  }
  return output;
}

function executeOne(current: number, program: number[]): Output {
  const currentDigits = String(program[current]).padStart(5, "0");
  const opcode = Number(currentDigits[3] + currentDigits[4]);
  const fstMod = currentDigits[2];
  const sndMod = currentDigits[1];
  const copy = program.slice();

  if (opcode == 1) {
    copy[copy[current + 3]] =
      getValue(program, current, 1, fstMod) +
      getValue(program, current, 2, sndMod);
    return {
      index: current + 4,
      program: copy,
      halt: false,
      output: null,
    };
  }
  if (opcode == 2) {
    copy[copy[current + 3]] =
      getValue(program, current, 1, fstMod) *
      getValue(program, current, 2, sndMod);
    return {
      index: current + 4,
      program: copy,
      halt: false,
      output: null,
    };
  }
  if (opcode == 3) {
    copy[copy[current + 1]] = 1;
    return {
      index: current + 2,
      program: copy,
      halt: false,
      output: null,
    };
  }
  if (opcode == 4) {
    copy[copy[current + 1]] = getValue(program, current, 1, fstMod);
    return {
      index: current + 2,
      program: copy,
      halt: false,
      output: copy[copy[current + 1]],
    };
  }
  if (opcode == 99) {
    return {
      index: 0,
      program: [],
      halt: true,
      output: null,
    };
  }
  throw Error(
    `Invalid opcode ${opcode} at ${current} in ${program.slice(
      current,
      current + 4
    )}`
  );
}

function getValue(
  program: number[],
  current: number,
  param: number,
  mode: string
) {
  if (mode === "1") return program[current + param];
  return program[program[current + param]];
}

Deno.readTextFile("day05.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
