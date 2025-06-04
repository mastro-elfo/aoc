function solution(content: string) {
  console.log(
    content
      .split("")
      .map((current) => (current === "(" ? 1 : -1))
      .reduce((acc, cur, index) => {
        if (index === 0) return [cur];
        return [...acc, acc[index - 1] + cur];
      }, [] as number[])
      .findIndex((item) => item === -1) + 1
  );
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution);
