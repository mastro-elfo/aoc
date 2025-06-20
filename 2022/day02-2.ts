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

function isRock(opponent: string, player: string) {
  return (
    (opponent === "B" && player === "X") ||
    (opponent === "A" && player === "Y") ||
    (opponent === "C" && player === "Z")
  );
}

function isPaper(opponent: string, player: string) {
  return (
    (opponent === "C" && player === "X") ||
    (opponent === "B" && player === "Y") ||
    (opponent === "A" && player === "Z")
  );
}

function parse(line: string) {
  const [o, p] = line.split(" ");
  return [
    o === "A" ? "R" : o === "B" ? "P" : "S",
    isRock(o, p) ? "R" : isPaper(o, p) ? "P" : "S",
  ];
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
