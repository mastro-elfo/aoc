type Limits = { start: number; end: number };
type Pair = { first: Limits; second: Limits };

function solution(content: string) {
  return content.split("\n").map(parse).filter(isContained).length;
}

function isContained({
  first: { start: fs, end: fe },
  second: { start: ss, end: se },
}: Pair): boolean {
  return (
    (ss <= fs && fs <= se && ss <= fe && fe <= se) ||
    (fs <= ss && ss <= fe && fs <= se && se <= fe)
  );
}

function parse(line: string): Pair {
  const match = line.match(/(\d+)-(\d+),(\d+)-(\d+)/);
  if (!match) throw new Error(`Invalid line: ${line}`);
  return {
    first: {
      start: Number(match[1]),
      end: Number(match[2]),
    },
    second: {
      start: Number(match[3]),
      end: Number(match[4]),
    },
  };
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
