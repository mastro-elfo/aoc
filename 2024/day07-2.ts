type Operation = "CAT" | "MUL" | "SUM";

type Equation = {
  result: number;
  args: number[];
};

function solution(content: string) {
  return content
    .split("\n")
    .map(parse)
    .filter(test)
    .map(({ result }) => result)
    .reduce((acc, cur) => acc + cur, 0);
}

function test(eq: Equation): boolean {
  return !!operations(eq.args.length - 1).find(
    (ops) => eq.result === apply(eq.args, ops)
  );
}

function apply(args: number[], ops: Operation[]): number {
  const [fst, ...rest] = args;
  return ops.reduce((acc, cur, index) => calc(acc, rest[index], cur), fst);
}

function calc(x: number, y: number, op: Operation): number {
  if (op === "CAT") return Number(`${x}${y}`);
  if (op === "MUL") return x * y;
  if (op === "SUM") return x + y;
  throw new Error(`Operation not supported: ${op}`);
}

function operations(n: number): Operation[][] {
  if (n === 0) return [[]];
  const recursive = operations(n - 1);
  return recursive
    .map((r) => ["MUL" as Operation, ...r])
    .concat(recursive.map((r) => ["SUM", ...r]))
    .concat(recursive.map((r) => ["CAT", ...r]));
}

function parse(line: string): Equation {
  return {
    result: Number(line.split(" ")[0].slice(0, -1)),
    args: line.split(" ").slice(1).map(Number),
  };
}

Deno.readTextFile("day07.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
