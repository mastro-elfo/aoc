function solution(content: string) {
  console.log(
    content.split("").filter((char) => char === "(").length -
      content.split("").filter((char) => char === ")").length
  );
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution);
