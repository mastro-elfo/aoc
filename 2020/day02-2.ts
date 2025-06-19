type Entity = [number, number, string, string];

function solution(content: string) {
  return content
    .split("\n")
    .map((item) => parse(item))
    .filter((item) => isValid(item)).length;
}

function isValid(entity: Entity) {
  const [fst, snd, char, password] = entity;
  return (
    (password[fst - 1] === char && password[snd - 1] !== char) ||
    (password[snd - 1] === char && password[fst - 1] !== char)
  );
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
