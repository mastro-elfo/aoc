function solution(content: string) {
  return Math.max(...content.split("\n").map((line) => parse(line)));
}

function parse(line: string) {
  const row = toInt(
    line
      .slice(0, 7)
      .split("")
      .map((x) => (x === "B" ? 1 : 0))
  );
  const col = toInt(
    line
      .slice(7)
      .split("")
      .map((x) => (x === "R" ? 1 : 0))
  );
  return row * 8 + col;
}

function toInt(binary: number[]) {
  function helper(mult: number, list: number[]): number {
    if (list.length === 0) return 0;
    const [first, ...rest] = list;
    if (first === 1) return mult + helper(2 * mult, rest);
    return helper(2 * mult, rest);
  }
  return helper(1, binary.slice().reverse());
}

Deno.readTextFile("day05.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
