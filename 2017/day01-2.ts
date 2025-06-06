function solution(content: string) {
  const ahead = Math.floor(content.length / 2);
  console.log(
    content
      .split("")
      .map((item, index, array) =>
        item === array[(index + ahead) % array.length] ? parseInt(item) : 0
      )
      .reduce((acc, cur) => acc + cur, 0)
  );
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution);
