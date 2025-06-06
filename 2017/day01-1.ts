function solution(content: string) {
  console.log(
    content
      .split("")
      .map((item, index, array) =>
        item === array[(index + 1) % array.length] ? parseInt(item) : 0
      )
      .reduce((acc, cur) => acc + cur, 0)
  );
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution);
