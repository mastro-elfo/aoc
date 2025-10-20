function solution(content: string) {
  return content
    .split("\n\n")
    .map((group) => everyYes(group.split("\n")).length)
    .reduce((acc, cur) => acc + cur, 0);
}

function everyYes(group: string[]) {
  return Array.from(new Set(group.join(""))).filter((answer) =>
    group.every((person) => person.includes(answer))
  );
}

Deno.readTextFile("day06.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
