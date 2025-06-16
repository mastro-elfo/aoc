function solution(content: string) {
  return content
    .split("\n")
    .map((line) => line.split("\t").map((item) => parseInt(item)))
    .map((line) => rowExactDivision(line))
    .reduce((acc, cur) => acc + cur, 0);
}

function rowExactDivision(line: number[]) {
  return line
    .map((item, index) =>
      valueExactDivision(item, [
        ...line.slice(0, index),
        ...line.slice(index + 1),
      ])
    )
    .find((item) => item)!;
}

function valueExactDivision(value: number, line: number[]) {
  return line
    .map((item) => (value % item === 0 ? value / item : null))
    .find((item) => item);
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
