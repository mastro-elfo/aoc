type Dimensions = [number, number, number];

function solution(content: string) {
  return content
    .split("\n")
    .map((line) => ribbon(parse(line)))
    .reduce((acc, cur) => acc + cur, 0);
}

function ribbon(dims: Dimensions) {
  const [a, b] = dims.toSorted((a, b) => a - b);
  return 2 * a + 2 * b + dims[0] * dims[1] * dims[2];
}

function parse(line: string): Dimensions {
  const dims = line.split("x").map((item) => parseInt(item));
  return [dims[0], dims[1], dims[2]];
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
