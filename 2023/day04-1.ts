type Scratchcard = {
  id: string;
  numbers: number[];
  winning: number[];
};

function solution(content: string) {
  return content
    .split("\n")
    .map(parse)
    .map(winning)
    .filter((item) => item > 0)
    .map((item) => Math.pow(2, item - 1))
    .reduce((acc, cur) => acc + cur, 0);
}

function winning(card: Scratchcard) {
  return card.winning.reduce(
    (acc, cur) => acc + (card.numbers.includes(cur) ? 1 : 0),
    0
  );
}

function parse(line: string): Scratchcard {
  const match = line.match(/^Card\s*(\d+):\s*([\d\s]+)\s*\|\s*([\d\s]+)$/);
  if (!match) throw new Error(`Invalid line ${line}`);
  return {
    id: match[1],
    numbers: match[2]
      .replace(/\s+/, " ")
      .split(" ")
      .filter((item) => item)
      .map(Number),
    winning: match[3]
      .replace(/\s+/, " ")
      .split(" ")
      .filter((item) => item)
      .map(Number),
  };
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
