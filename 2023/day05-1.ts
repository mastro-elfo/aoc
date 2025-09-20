type Table = {
  dst: number;
  src: number;
  size: number;
};

function solution(content: string) {
  const lines = content.split("\n");
  const maps = parse(lines.slice(2));
  const seeds = parseSeeds(lines[0]);

  return Math.min(...seeds.map((seed) => convert(seed, maps)));
}

function parseSeeds(line: string) {
  return line.slice(7).split(" ").map(Number);
}

function parse(lines: string[]): Table[][] {
  const output: Table[][] = [];
  lines.forEach((line) => {
    if (line.trim() === "") return;
    if (line.includes("-")) output.push([]);
    else output.at(-1)?.push(parseOne(line));
  });
  return output;
}

function parseOne(line: string): Table {
  const values = line.split(" ").map(Number);
  return {
    dst: values[0],
    src: values[1],
    size: values[2],
  };
}

function convert(seed: number, maps: Table[][]): number {
  if (maps.length === 0) return seed;
  const [fst, ...rest] = maps;
  const map = fst.find(({ src, size }) => src <= seed && seed < src + size);
  if (map) return convert(map.dst + seed - map.src, rest);
  return convert(seed, rest);
}

Deno.readTextFile("day05.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
