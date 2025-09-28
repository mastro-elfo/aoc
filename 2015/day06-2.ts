type Lights = number[][];
type Instruction = {
  action: "on" | "off" | "toggle";
  sx: number;
  sy: number;
  ex: number;
  ey: number;
};

function solution(content: string) {
  let lights: Lights = Array.from({ length: 1000 }).map((_) =>
    Array.from({ length: 1000 }).map((_) => 0)
  );
  content
    .split("\n")
    .map(parse)
    .forEach((instruction) => {
      lights = act(instruction, lights);
    });
  return lights.reduce(
    (acc, cur) => acc + cur.reduce((acc2, cur2) => acc2 + cur2, 0),
    0
  );
}

function act(instruction: Instruction, lights: Lights): Lights {
  const { action, sx, sy, ex, ey } = instruction;
  const copy = lights.slice();
  for (let x = sx; x <= ex; x++) {
    for (let y = sy; y <= ey; y++) {
      if (action === "off") copy[x][y] = Math.max(0, copy[x][y] - 1);
      if (action === "on") copy[x][y] += 1;
      if (action === "toggle") copy[x][y] += 2;
    }
  }
  return copy;
}

function parse(line: string): Instruction {
  const match = line.match(
    /(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)/
  );
  if (!match) throw new Error(`Invalid line: ${line}`);
  return {
    action:
      match[1] === "turn on"
        ? "on"
        : match[1] === "turn off"
        ? "off"
        : "toggle",
    sx: Number(match[2]),
    sy: Number(match[3]),
    ex: Number(match[4]),
    ey: Number(match[5]),
  };
}

Deno.readTextFile("day06.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
