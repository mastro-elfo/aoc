type Operation = "SIG" | "NOT" | "AND" | "OR" | "LSHIFT" | "RSHIFT";

type Instruction = {
  left: string;
  right: string;
  operation: Operation;
  target: string;
};

let instructions: Instruction[] = [];

function solution(content: string) {
  instructions = content.split("\n").map(parse);
  //   Remove instruction to wire 'b'
  instructions = instructions.filter(({ target }) => target !== "b");
  // Add a new instruction for wire 'b', with the result from part 1
  instructions.push({ left: "3176", right: "", operation: "SIG", target: "b" });
  return evaluate(target("a"));
}

const cache: Record<string, number> = {};

function hash({ left, right, operation }: Omit<Instruction, "target">) {
  return `${left}-${right}-${operation}`;
}

function evaluate(instruction: Instruction): number {
  const helper = ({ left, right, operation }: Instruction) => {
    if (operation === "SIG" && isNumber(left)) return Number(left);
    if (operation === "SIG") return evaluate(target(left));
    if (operation === "NOT" && isNumber(left)) return norm(~Number(left));
    if (operation === "NOT") return norm(~evaluate(target(left)));
    if (operation === "AND" && isNumber(left) && isNumber(right))
      return Number(left) & Number(right);
    if (operation === "AND" && isNumber(left))
      return Number(left) & evaluate(target(right));
    if (operation === "AND" && isNumber(right))
      return evaluate(target(left)) & Number(right);
    if (operation === "AND")
      return evaluate(target(left)) & evaluate(target(right));
    if (operation === "OR" && isNumber(left) && isNumber(right))
      return Number(left) | Number(right);
    if (operation === "OR" && isNumber(left))
      return Number(left) | evaluate(target(right));
    if (operation === "OR" && isNumber(right))
      return evaluate(target(left)) | Number(right);
    if (operation === "OR")
      return evaluate(target(left)) | evaluate(target(right));
    if (operation === "LSHIFT" && isNumber(left))
      return Number(left) << Number(right);
    if (operation === "LSHIFT") return evaluate(target(left)) << Number(right);
    if (operation === "RSHIFT" && isNumber(left))
      return Number(left) >> Number(right);
    if (operation === "RSHIFT") return evaluate(target(left)) >> Number(right);
    throw new Error(`Invalid operation ${operation}`);
  };

  const hashKey = hash(instruction);
  if (cache[hashKey] === undefined) {
    cache[hashKey] = helper(instruction);
  }
  return cache[hashKey];
}

function norm(value: number) {
  return value < 0 ? 65536 + value : value;
}

function target(trg: string): Instruction {
  return instructions.find((t) => t.target === trg)!;
}

function parse(line: string): Instruction {
  const parts = line.split(" ");
  if (parts.includes("NOT")) {
    return {
      left: parts[1],
      right: "",
      operation: "NOT",
      target: parts.at(-1)!,
    };
  }
  if (parts.includes("AND")) {
    return {
      left: parts[0],
      right: parts[2],
      operation: "AND",
      target: parts.at(-1)!,
    };
  }
  if (parts.includes("OR")) {
    return {
      left: parts[0],
      right: parts[2],
      operation: "OR",
      target: parts.at(-1)!,
    };
  }
  if (parts.includes("LSHIFT")) {
    return {
      left: parts[0],
      right: parts[2],
      operation: "LSHIFT",
      target: parts.at(-1)!,
    };
  }
  if (parts.includes("RSHIFT")) {
    return {
      left: parts[0],
      right: parts[2],
      operation: "RSHIFT",
      target: parts.at(-1)!,
    };
  }
  return {
    left: parts[0],
    right: "",
    operation: "SIG",
    target: parts.at(-1)!,
  };
}

function isNumber(value: string) {
  return value.split("").every((ch) => "1234567890".includes(ch));
}

Deno.readTextFile("day07.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
