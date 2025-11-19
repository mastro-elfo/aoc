type State = {
  inputs: number[];
  current: number;
  program: number[];
  halt: boolean;
  output: number;
};

function solution(content: string) {
  const program = content.split(",").map(Number);
  const phases: number[][] = [];
  console.log(permutations([0, 1, 2, 3, 4]));
  return Math.max(...phases.map((setting) => amplify(program, setting)));
}

function permutations<T>(list: T[]): T[][] {
  if (list.length === 0) return [] as T[][];
  return list.map((item, _, array) =>
    permutations(array.slice(1))
      .map((items) => [item, ...items])
      .flat()
  );
}

function amplify(program: number[], settings: number[]) {
  return settings.reduce((acc, cur) => run(program, [cur, acc]), 0);
}

function run(program: number[], inputs: number[]) {
  let state: State = {
    inputs,
    current: 0,
    program: program.slice(),
    halt: false,
    output: 0,
  };
  let { halt, output } = state;
  while (!halt) {
    state = execute(state);
    halt = state.halt;
    output = state.output;
  }
  return output;
}

function execute(state: State): State {
  const { inputs, current, program, output } = state;

  const digits = String(program[current]).padStart(5, "0");
  const opcode = digits[3] + digits[4];
  const fstMod = digits[2];
  const sndMod = digits[1];
  const copy = program.slice();

  if (opcode === "01") {
    copy[copy[current + 3]] =
      getValue(program, current, 1, fstMod) +
      getValue(program, current, 2, sndMod);
    return {
      inputs,
      current: current + 4,
      program: copy,
      halt: false,
      output,
    };
  }
  if (opcode === "02") {
    copy[copy[current + 3]] =
      getValue(program, current, 1, fstMod) *
      getValue(program, current, 2, sndMod);
    return {
      inputs,
      current: current + 4,
      program: copy,
      halt: false,
      output,
    };
  }
  if (opcode === "03") {
    copy[copy[current + 1]] = inputs[0];
    return {
      inputs: inputs.length === 1 ? inputs : inputs.slice(1),
      current: current + 2,
      program: copy,
      halt: false,
      output,
    };
  }
  if (opcode === "04") {
    copy[copy[current + 1]] = getValue(program, current, 1, fstMod);
    return {
      inputs,
      current: current + 2,
      program: copy,
      halt: false,
      output: copy[copy[current + 1]],
    };
  }
  if (opcode == "99") {
    return {
      inputs,
      current,
      program,
      halt: true,
      output,
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

Deno.readTextFile("day07.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
