type Dimensions = [number, number, number];

function solution(content: string) {
  return content
    .split("\n")
    .map((line) => paper(parse(line)))
    .reduce((acc, cur) => acc + cur, 0);
}

function paper(dims: Dimensions) {
  const a = dims[0] * dims[1];
  const b = dims[1] * dims[2];
  const c = dims[0] * dims[2];
  return 2 * a + 2 * b + 2 * c + Math.min(a, b, c);
}

function parse(line: string): Dimensions {
  const dims = line.split("x").map((item) => parseInt(item));
  return [dims[0], dims[1], dims[2]];
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
