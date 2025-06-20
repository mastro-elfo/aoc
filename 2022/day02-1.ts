function solution(content: string) {
  return content
    .split("\n")
    .map(parse)
    .map(play)
    .reduce((acc, cur) => acc + cur, 0);
}

function play([opponent, player]: string[]) {
  return (
    (player === "R" ? 1 : player === "P" ? 2 : 3) +
    (isLosing(opponent, player) ? 0 : opponent === player ? 3 : 6)
  );
}

function isLosing(opponent: string, player: string) {
  return (
    (opponent === "R" && player === "S") ||
    (opponent === "S" && player === "P") ||
    (opponent === "P" && player === "R")
  );
}

function parse(line: string) {
  const [opponent, player] = line.split(" ");
  return [
    opponent === "A" ? "R" : opponent === "B" ? "P" : "S",
    player === "X" ? "R" : player === "Y" ? "P" : "S",
  ];
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
