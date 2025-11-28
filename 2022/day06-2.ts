function solution(content: string) {
  return (
    14 +
    content
      .split("")
      .map(
        (_, index, array) =>
          !repeating([
            array[index],
            array[index + 1],
            array[index + 2],
            array[index + 3],
            array[index + 4],
            array[index + 5],
            array[index + 6],
            array[index + 7],
            array[index + 8],
            array[index + 9],
            array[index + 10],
            array[index + 11],
            array[index + 12],
            array[index + 13],
          ])
      )
      .findIndex((v) => v)
  );
}

function repeating(values: string[]): boolean {
  const [first, ...rest] = values;
  if (rest.length === 0) return false;
  return rest.includes(first) || repeating(rest);
}

Deno.readTextFile("day06.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
