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
    .filter(isPossible)
    .map((game) => game.id)
    .reduce((acc, cur) => acc + cur, 0);
}

function isPossible(game: Game) {
  return game.colors.every(isValid);
}

function isValid(colors: Colors) {
  return colors.red <= 12 && colors.green <= 13 && colors.blue <= 14;
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
