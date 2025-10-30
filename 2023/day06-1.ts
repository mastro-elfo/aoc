type Data = {
  time: number;
  dist: number;
};

function solution(content: string) {
  return parse(content).reduce((acc, cur) => acc * winning(cur), 1);
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

function parse(content: string): Data[] {
  const [fst, snd] = content.split("\n");
  const times = fst.replaceAll(/\s+/g, " ").split(" ").slice(1);
  const dists = snd.replaceAll(/\s+/g, " ").split(" ").slice(1);
  return times.map((t, index) => ({
    time: Number(t),
    dist: Number(dists[index]),
  }));
}

Deno.readTextFile("day06.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
