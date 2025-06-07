function solution(content: string[]) {
  return content.map(Number).reduce((acc, cur) => acc + cur, 0);
}

Deno.readTextFile("day01.dat")
  .then((content) => content.split("\n"))
  .then(solution)
  .then(console.log);
