function solution(content: string) {
  const count = content.split("\n").map((line) => [has2x(line), has3x(line)]);
  return count.filter(([x]) => x).length * count.filter(([, x]) => x).length;
}

function has2x(line: string) {
  return line
    .split("")
    .map((y) => line.split("").filter((x) => x == y).length)
    .filter((z) => z == 2)
    .some((t) => t);
}

function has3x(line: string) {
  return line
    .split("")
    .map((y) => line.split("").filter((x) => x == y).length)
    .filter((z) => z == 3)
    .some((t) => t);
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
