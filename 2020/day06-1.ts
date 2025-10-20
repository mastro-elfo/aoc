function solution(content: string) {
  return content
    .split("\n\n")
    .map((group) => new Set(group.replaceAll("\n", "")).size)
    .reduce((acc, cur) => acc + cur, 0);
}

Deno.readTextFile("day06.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
