function solution(content: string) {
  return content
    .split("\n")
    .map((line) => [...line.matchAll(/\d+/g)].map(Number))
    .filter(([a, b, c]) => isTriangle(a, b, c)).length;
}

function isTriangle(a: number, b: number, c: number) {
  return a + b > c && a + c > b && b + c > a;
}

Deno.readTextFile("day03.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
