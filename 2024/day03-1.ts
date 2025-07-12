function solution(content: string) {
  return content
    .matchAll(/mul\((\d+),(\d+)\)/g)
    .map(([_, a, b]) => Number(a) * Number(b))
    .reduce((acc, cur) => acc + cur, 0);
}

Deno.readTextFile("day03.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
