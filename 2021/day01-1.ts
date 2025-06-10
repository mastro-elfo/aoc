function solution(content: string) {
  const depths = content.split("\n").map((item) => parseInt(item));
  return depths
    .slice(1)
    .map((item, index) => item > depths[index])
    .filter((item) => item).length;
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
