type State = {
  inputs: number[];
  current: number;
  program: number[];
  halt: boolean;
  suspend: boolean;
  output: number;
};

function solution(content: string) {
  const program = content.split(",").map(Number);
  const phases: number[][] = permutations([5, 6, 7, 8, 9]);
  return Math.max(...phases.map((setting) => amplify(program, setting)));
}

function permutations<T>(list: T[]): T[][] {
  if (list.length === 0) return [] as T[][];
  if (list.length === 1) return [[list[0]]];
  return list
    .map((item) =>
      permutations(list.filter((itm) => itm !== item)).map((p) => [item, ...p])
    )
    .flat();
}

function reset(state: State, inputs: number[]): State {
  return { ...state, suspend: false, inputs };
}

function amplify(program: number[], settings: number[]) {
  const [pa, pb, pc, pd, pe] = settings;

  let stateA = {
    inputs: [pa, 0],
    current: 0,
    program: program.slice(),
    suspend: false,
    halt: false,
    output: 0,
  };
  stateA = run(stateA);
  let stateB = {
    inputs: [pb, stateA.output],
    current: 0,
    program: program.slice(),
    suspend: false,
    halt: false,
    output: 0,
  };
  stateB = run(stateB);
  let stateC = {
    inputs: [pc, stateB.output],
    current: 0,
    program: program.slice(),
    suspend: false,
    halt: false,
    output: 0,
  };
  stateC = run(stateC);
  let stateD = {
    inputs: [pd, stateC.output],
    current: 0,
    program: program.slice(),
    suspend: false,
    halt: false,
    output: 0,
  };
  stateD = run(stateD);
  let stateE = {
    inputs: [pe, stateD.output],
    current: 0,
    program: program.slice(),
    suspend: false,
    halt: false,
    output: 0,
  };
  stateE = run(stateE);

  while ([stateA, stateB, stateC, stateD, stateE].some(({ halt }) => !halt)) {
    stateA = run(reset(stateA, [stateE.output]));
    stateB = run(reset(stateB, [stateA.output]));
    stateC = run(reset(stateC, [stateB.output]));
    stateD = run(reset(stateD, [stateC.output]));
    stateE = run(reset(stateE, [stateD.output]));
  }

  return stateE.output;
}

function run(state: State) {
  let { halt, suspend } = state;
  if (halt) return state;
  while (!suspend) {
    state = execute(state);
    suspend = state.suspend;
  }
  return state;
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
      suspend: false,
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
      suspend: false,
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
      suspend: false,
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
      suspend: true,
      output: copy[copy[current + 1]],
    };
  }
  if (opcode === "05") {
    if (getValue(program, current, 1, fstMod) !== 0) {
      return {
        inputs,
        current: getValue(program, current, 2, sndMod),
        program: copy,
        halt: false,
        suspend: false,
        output,
      };
    }
    return {
      inputs,
      current: current + 3,
      program: copy,
      halt: false,
      suspend: false,
      output,
    };
  }
  if (opcode === "06") {
    if (getValue(program, current, 1, fstMod) === 0) {
      return {
        inputs,
        current: getValue(program, current, 2, sndMod),
        program: copy,
        halt: false,
        suspend: false,
        output,
      };
    }
    return {
      inputs,
      current: current + 3,
      program: copy,
      halt: false,
      suspend: false,
      output,
    };
  }
  if (opcode === "07") {
    copy[copy[current + 3]] =
      getValue(program, current, 1, fstMod) <
      getValue(program, current, 2, sndMod)
        ? 1
        : 0;
    return {
      inputs,
      current: current + 4,
      program: copy,
      halt: false,
      suspend: false,
      output,
    };
  }
  if (opcode === "08") {
    copy[copy[current + 3]] =
      getValue(program, current, 1, fstMod) ===
      getValue(program, current, 2, sndMod)
        ? 1
        : 0;
    return {
      inputs,
      current: current + 4,
      program: copy,
      halt: false,
      suspend: false,
      output,
    };
  }
  if (opcode === "99") {
    return {
      inputs,
      current,
      program,
      halt: true,
      suspend: true,
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
