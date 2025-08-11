type Scratchcard = {
  id: string;
  numbers: number[];
  winning: number[];
  count: number;
};

function solution(content: string) {
  return play(content.split("\n").map(parse)).reduce(
    (acc, cur) => acc + cur.count,
    0
  );
}

function play(cards: Scratchcard[]) {
  const output = cards.slice();
  cards.forEach((_, index) => {
    const wins = winning(output[index]);
    for (let i = 1; i < wins + 1; i++) {
      output[index + i] = {
        ...output[index + i],
        count: output[index + i].count + output[index].count,
      };
    }
  });
  return output;
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
    count: 1,
  };
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
