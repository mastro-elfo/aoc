type Count = { c0: number; c1: number };

function solution(content: string) {
  const parsed = content
    .split("\n")
    .map((line) => line.trim().split("").map(Number));

  return (
    toInt(filterMostCommon(parsed, 0)) * toInt(filterLeastCommon(parsed, 0))
  );
}

function filterMostCommon(content: number[][], at: number) {
  if (content.length === 1) return content[0];
  const count = countAll(content);
  const { c0, c1 } = count[at];
  return filterMostCommon(
    content.filter((line) => line[at] === (c0 > c1 ? 0 : 1)),
    at + 1
  );
}

function filterLeastCommon(content: number[][], at: number) {
  if (content.length === 1) return content[0];
  const count = countAll(content);
  const { c0, c1 } = count[at];
  return filterLeastCommon(
    content.filter((line) => line[at] === (c0 <= c1 ? 0 : 1)),
    at + 1
  );
}

function toInt(binary: number[]) {
  return binary
    .reverse()
    .reduce((acc, cur, index) => acc + cur * Math.pow(2, index), 0);
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
