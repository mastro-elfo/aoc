function solution(content: string) {
  const [fst, snd] = content.split("\n").reduce((acc, fst) => {
    if (acc) return acc;
    const similar = content
      .split("\n")
      .filter((snd) => distance(fst, snd) === 1);
    if (similar.length) return [fst, similar.at(0)!];
    return null;
  }, null as [string, string] | null)!;

  return fst
    .split("")
    .map((a, index) => (a === snd[index] ? a : ""))
    .join("");
}

function distance(fst: string, snd: string) {
  return fst
    .split("")
    .map<number>((a, index) => (a === snd[index] ? 0 : 1))
    .reduce((acc, cur) => acc + cur, 0);
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
