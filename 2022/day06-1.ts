function solution(content: string) {
  return (
    4 +
    content
      .split("")
      .map(
        (_char, index, array) =>
          !repeating(
            array[index],
            array[index + 1],
            array[index + 2],
            array[index + 3]
          )
      )
      .findIndex((v) => v)
  );
}

function repeating(a: string, b: string, c: string, d: string): boolean {
  return a === b || a === c || a === d || b === c || b === d || c == d;
}

Deno.readTextFile("day06.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
