// TODO: Test between best and worst case
// TODO: Region has a width and height

type Region = {
  area: number;
  requests: number[];
};

function solution(content: string) {
  const parts = content.split("\n\n");
  const tiles = parts.slice(0, -1).map(parseTile);
  const regions = parts.at(-1)!.split("\n").map(parseRegion);
  return regions.filter((region) => test(tiles, region)).length;
}

function test(sizes: number[], region: Region): boolean {
  const { area, requests } = region;
  return (
    area >= requests.reduce((acc, cur, index) => acc + cur * sizes[index], 0)
  );
}

function parseTile(part: string): number {
  return part.split("").filter((item) => item === "#").length;
}

function parseRegion(part: string): Region {
  const matches = part.match(/(\d+)x(\d+): (.+)/);
  if (matches === null) throw new Error(`Invalid line: ${part}`);
  return {
    area: Number(matches[1]) * Number(matches[2]),
    requests: matches[3].split(" ").map(Number),
  };
}

Deno.readTextFile("day12.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
