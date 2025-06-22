type Position = {
  x: number;
  y: number;
};

function solution(content: string) {
  return content
    .split("")
    .reduce(
      (acc, direction, index) =>
        index % 2 === 0 ? [...acc, move(acc.at(-1)!, direction)] : acc,
      [{ x: 0, y: 0 }]
    )
    .concat(
      content
        .split("")
        .reduce(
          (acc, direction, index) =>
            index % 2 !== 0 ? [...acc, move(acc.at(-1)!, direction)] : acc,
          [{ x: 0, y: 0 }]
        )
    )
    .filter(
      (item, index, array) =>
        !array
          .slice(0, index)
          .find((itm) => itm.x === item.x && itm.y === item.y)
    ).length;
}

function move({ x, y }: Position, direction: string) {
  if (direction === "^") return { x, y: y + 1 };
  if (direction === "v") return { x, y: y - 1 };
  if (direction === "<") return { x: x - 1, y };
  if (direction === ">") return { x: x + 1, y };
  throw new Error(`Invalid direction ${direction}`);
}

Deno.readTextFile("day03.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
