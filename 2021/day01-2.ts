function solution(content: string) {
  const depths = content.split("\n").map((item) => parseInt(item));
  const windows = depths
    .slice(2)
    .map((item, index) => item + depths[index] + depths[index + 1]);
  return windows
    .slice(1)
    .map((item, index) => item > windows[index])
    .filter((item) => item).length;
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
