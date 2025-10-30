type Data = {
  time: number;
  dist: number;
};

function solution(content: string) {
  return winning(parse(content));
}

function winning(pair: Data) {
  const { time, dist } = pair;
  const delta = Math.sqrt(time * time - 4 * dist) / 2;
  const halfT = time / 2;
  const intersect = halfT + delta;
  return (
    Math.floor(halfT + delta) -
    Math.ceil(halfT - delta) +
    1 -
    (Math.ceil(intersect) === Math.floor(intersect) ? 2 : 0)
  );
}

function parse(content: string) {
  const [fst, snd] = content.split("\n");
  return {
    time: Number(fst.replaceAll(/\s+/g, " ").split(" ").slice(1).join("")),
    dist: Number(snd.replaceAll(/\s+/g, " ").split(" ").slice(1).join("")),
  };
}

Deno.readTextFile("day06.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
