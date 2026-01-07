// FIXME: This solution should work but it's very slow

type Table = {
  dst: number;
  src: number;
  size: number;
};

type Range = {
  start: number;
  size: number;
};

function solution(content: string) {
  const maps = parse(content.split("\n").slice(2));
  const seeds = parseSeeds(content.split("\n")[0]);
  return Math.min(...seeds.map((range) => process(range, maps)));
}

function process(range: Range, maps: Table[][]): number {
  return Array.from({ length: range.size }).reduce(
    (acc: number | null, _, index) => {
      const loc = convert(index + range.start, maps);
      return acc === null ? loc : Math.min(loc, acc);
    },
    null
  )!;
}

function parseSeeds(line: string): Range[] {
  const values = line.slice(7).split(" ").map(Number);
  return values
    .filter((_, index) => index % 2 === 0)
    .map((start, index) => ({ start, size: values[index * 2 + 1] }));
}

function parse(lines: string[]): Table[][] {
  return lines.reduce((acc, cur) => {
    if (cur.trim() === "") return acc;
    if (cur.includes("-")) return [...acc, []];
    return [...acc.slice(0, -1), acc.at(-1)!.concat([parseOne(cur)])];
  }, [] as Table[][]);
}

function parseOne(line: string): Table {
  const [dst, src, size] = line.split(" ").map(Number);
  return {
    dst,
    src,
    size,
  };
}

function convert(seed: number, maps: Table[][]): number {
  if (maps.length === 0) return seed;
  const [fst, ...rest] = maps;
  const rule = fst.find(({ src, size }) => src <= seed && seed < src + size);
  if (rule) {
    return convert(rule.dst + seed - rule.src, rest);
  }
  return convert(seed, rest);
}

Deno.readTextFile("day05.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
