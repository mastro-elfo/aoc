type Coord = {
  x: number;
  y: number;
};

type Component = {
  value: number;
  coords: Coord[];
};

function solution(content: string) {
  const components = parseComponents(content);
  const stars = parseStars(content);
  const gears = stars.filter(
    (star) =>
      components.filter(({ coords }) =>
        surroundings(coords).find((sur) => sur.x === star.x && sur.y === star.y)
      ).length === 2
  );
  return gears
    .map((gear) =>
      components
        .filter(({ coords }) =>
          surroundings(coords).find(
            (sur) => sur.x === gear.x && sur.y === gear.y
          )
        )
        .reduce((acc, { value }) => acc * value, 1)
    )
    .reduce((acc, cur) => acc + cur, 0);
}

function surroundings(coords: Coord[]) {
  return coords
    .map(({ x, y }) => [
      { x: x - 1, y: y - 1 },
      { x: x - 1, y: y },
      { x: x - 1, y: y + 1 },
      { x, y: y - 1 },
      { x, y: y + 1 },
      { x: x + 1, y: y - 1 },
      { x: x + 1, y: y },
      { x: x + 1, y: y + 1 },
    ])
    .flat();
}

function parseStars(content: string) {
  return content
    .split("\n")
    .map((row, rowIndex) =>
      row
        .split("")
        .map((char, colIndex) =>
          char === "*" ? { x: colIndex, y: rowIndex } : null
        )
    )
    .flat()
    .filter((item) => item) as Coord[];
}

function parseComponents(content: string) {
  let component: Component | null = null;
  const output: Component[] = [];
  content.split("\n").forEach((row, rowIndex) => {
    row.split("").forEach((char, colIndex) => {
      if ("1234567890".includes(char)) {
        if (component) {
          component.value = component.value * 10 + Number(char);
          component.coords.push({ x: colIndex, y: rowIndex });
        } else {
          component = {
            value: Number(char),
            coords: [{ x: colIndex, y: rowIndex }],
          };
        }
      } else if (component) {
        output.push(component);
        component = null;
      }
    });
    if (component) {
      output.push(component);
      component = null;
    }
  });
  return output;
}

Deno.readTextFile("day03.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
