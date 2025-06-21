type Colors = {
  red: number;
  green: number;
  blue: number;
};

type Game = {
  id: number;
  colors: Colors[];
};

function solution(content: string) {
  return content
    .split("\n")
    .map(parse)
    .map(minimum)
    .map(power)
    .reduce((acc, cur) => acc + cur, 0);
}

function minimum(game: Game) {
  return {
    red: Math.max(...game.colors.map((colors) => colors.red)),
    green: Math.max(...game.colors.map((colors) => colors.green)),
    blue: Math.max(...game.colors.map((colors) => colors.blue)),
  };
}

function power(colors: Colors) {
  return colors.red * colors.green * colors.blue;
}

function parse(line: string): Game {
  const match = line.match(/Game (\d+): (.+)/);
  if (match === null) {
    throw new Error(`Invalid line: ${line}`);
  }
  return {
    id: Number(match[1]),
    colors: match[2].split(";").map(parseCubes),
  };
}

function parseCubes(cubes: string): Colors {
  const red = cubes.match(/(\d+) red/);
  const green = cubes.match(/(\d+) green/);
  const blue = cubes.match(/(\d+) blue/);
  return {
    red: Number(red !== null ? red[1] : 0),
    green: Number(green !== null ? green[1] : 0),
    blue: Number(blue !== null ? blue[1] : 0),
  };
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
