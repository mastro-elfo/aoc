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
        .some(
          (item, index, array) =>
            index < array.length - 1 && item === array[index + 1]
        )
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
