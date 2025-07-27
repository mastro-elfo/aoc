function solution(content: string) {
  return generate(content)
    .map(String)
    .filter((num) =>
      num
        .split("")
        .every(
          (item, index, array) =>
            index >= array.length - 1 || item <= array[index + 1]
        )
    )
    .filter((num) =>
      num
        .split("")
        .reduce(
          (acc, cur) =>
            acc.at(-1)?.includes(cur)
              ? [...acc.slice(0, -1), acc.at(-1) + cur]
              : [...acc, cur],
          [] as string[]
        )
        .some((item) => item.length == 2)
    ).length;
}

function generate(content: string) {
  const [lo, hi] = content.split("-").map(Number);
  return Array.from({ length: hi - lo }, (_, index) => lo + index);
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
