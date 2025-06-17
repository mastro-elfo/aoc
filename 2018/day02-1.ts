function solution(content: string) {
  const count = content
    .split("\n")
    .map((line) => [hasnx(2, line), hasnx(3, line)]);
  return count.filter(([x]) => x).length * count.filter(([, x]) => x).length;
}

function hasnx(n: number, line: string) {
  return line
    .split("")
    .map((y) => line.split("").filter((x) => x == y).length)
    .filter((z) => z == n)
    .some((t) => t);
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
