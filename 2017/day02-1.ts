function solution(content: string) {
  return content
    .split("\n")
    .map((line) => line.split("\t").map((item) => parseInt(item)))
    .map((line) => Math.max(...line) - Math.min(...line))
    .reduce((acc, cur) => acc + cur, 0);
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
