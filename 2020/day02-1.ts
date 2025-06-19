type Entity = [number, number, string, string];

function solution(content: string) {
  return content
    .split("\n")
    .map((item) => parse(item))
    .filter((item) => isValid(item)).length;
}

function isValid(entity: Entity) {
  const [min, max, char, password] = entity;
  const filtered = password.split("").filter((ch) => ch === char);
  return min <= filtered.length && filtered.length <= max;
}

function parse(line: string): Entity {
  const match = line.match(/(\d+)\-(\d+) ([a-z]): ([a-z]+)/);
  if (match === null) {
    throw Error(`Invalid line: ${line}`);
  }
  return [Number(match[1]), Number(match[2]), match[3], match[4]];
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
