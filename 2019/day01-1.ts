function solution(content: string) {
  return content
    .split("\n")
    .map((item) => Math.floor(parseInt(item) / 3) - 2)
    .reduce((acc, cur) => acc + cur, 0);
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
