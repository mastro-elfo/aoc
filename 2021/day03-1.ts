type Count = { c0: number; c1: number };

function solution(content: string) {
  const count = countAll(
    content.split("\n").map((line) => line.trim().split("").map(Number))
  );
  return toInt(mostCommonValue(count)) * toInt(leastCommonValue(count));
}

function toInt(binary: number[]) {
  return binary
    .reverse()
    .reduce((acc, cur, index) => acc + cur * Math.pow(2, index), 0);
}

function mostCommonValue(count: Count[]) {
  return count.map(({ c0, c1 }) => (c0 > c1 ? 0 : 1));
}
function leastCommonValue(count: Count[]) {
  return count.map(({ c0, c1 }) => (c0 < c1 ? 0 : 1));
}

function countAll(content: number[][]) {
  return content.reduce(
    (acc, cur) =>
      acc.map(({ c0, c1 }, index) => ({
        c0: c0 + (cur[index] === 0 ? 1 : 0),
        c1: c1 + (cur[index] === 1 ? 1 : 0),
      })),
    Array.from({ length: content[0].length }).map(() => ({
      c0: 0,
      c1: 0,
    })) as Count[]
  );
}

Deno.readTextFile("day03.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
