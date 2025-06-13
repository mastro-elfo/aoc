function solution(content: string) {
  const split = content
    .split("\n")
    .map((line) => line.split("   ").map((item) => parseInt(item)));
  const left = split.map((item) => item.at(0)!);
  const right = split.map((item) => item.at(1)!);
  return left
    .map((l) => l * right.filter((r) => r === l).length)
    .reduce((acc, cur) => acc + cur, 0);
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
