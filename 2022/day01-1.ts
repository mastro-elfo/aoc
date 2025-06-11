function solution(content: string) {
  return content
    .split("\n")
    .reduce(
      (acc, cur) => {
        if (cur.trim() === "") {
          return [...acc, []];
        } else {
          acc.at(-1)?.push(parseInt(cur));
          return [...acc];
        }
      },
      [[]] as number[][]
    )
    .map((elf) => elf.reduce((cur, acc) => cur + acc, 0))
    .toSorted((a, b) => b - a)
    .at(0);
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
