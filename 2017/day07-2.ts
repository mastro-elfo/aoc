type Program = {
  name: string;
  weight: number;
  programs: string[];
};

function solution(content: string) {
  const programs = content.split("\n").map(parse);

  const unblncd = unbalanced(programs);
  const wghts = weights(programs, unblncd);
  const hist = histogram(wghts);

  const minKey = Number(
    Object.keys(hist).find((key) => hist[Number(key)] === 1)
  );
  const othKey = Number(
    Object.keys(hist).find((key) => hist[Number(key)] !== 1)
  );
  const minIndex = wghts.indexOf(minKey);

  return get(programs, unblncd.programs[minIndex]).weight + othKey - minKey;
}

function unbalanced(programs: Program[]): Program {
  let unblncd = programs.filter((prog) =>
    isUnbalanced(weights(programs, prog))
  );
  let length = unblncd.length;
  while (unblncd.length > 1) {
    const names = unblncd.map(({ name }) => name);
    const topNames = new Set(unblncd.map(({ programs }) => programs).flat());
    const bottomNames = names.filter((name) => !topNames.has(name));
    unblncd = unblncd.filter((prog) => !bottomNames.includes(prog.name));
    if (unblncd.length === length) {
      throw new Error("Infinite loop detected");
    }
    length = unblncd.length;
  }
  return unblncd[0];
}

function histogram(nums: number[]): Record<number, number> {
  const hist: Record<number, number> = {};
  nums.forEach((num) => (hist[num] = (hist[num] ?? 0) + 1));
  return hist;
}

function isUnbalanced(nums: number[]) {
  if (nums.length === 0) return false;
  const [first, ...rest] = nums;
  return rest.some((value) => value !== first);
}

function parse(line: string): Program {
  const match = line.match(/(\w+) \((\d+)\)( -> ([\w\s,]+))?/);
  if (!match) {
    throw new Error(`Invalid line: ${line}`);
  }
  return {
    name: match[1],
    weight: Number(match[2]),
    programs: match[4] ? match[4].trim().split(", ") : [],
  };
}

function get(programs: Program[], name: string): Program {
  return programs.find((prog) => prog.name === name)!;
}

function weight(
  programs: Program[],
  { weight: w, programs: cnames }: Program
): number {
  const children = programs.filter((prog) => cnames.includes(prog.name));
  return (
    w +
    children
      .map((child) => weight(programs, child))
      .reduce((acc, cur) => acc + cur, 0)
  );
}

function weights(programs: Program[], root: Program) {
  if (!root) return [];
  const { programs: cnames } = root;
  const children = cnames.map((name) => get(programs, name));
  return children.map((child) => weight(programs, child));
}

Deno.readTextFile("day07.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
