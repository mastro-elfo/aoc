function solution(content: string) {
  return content
    .split("\n")
    .map(
      (line) =>
        firstDigit(line) * 10 + firstDigit(line.split("").reverse().join(""))
    )
    .reduce((acc, cur) => acc + cur, 0);
}

function firstDigit(line: string) {
  return parseInt(
    line.split("").find((ch) => "1234567890".includes(ch)) ?? "-1"
  );
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
