function solution(content: string) {
  return content
    .split("\n")
    .map((item) => parseInt(item))
    .toSorted()
    .map((e, index, array) => {
      const found = find(2020 - e, [
        ...array.slice(0, index),
        ...array.slice(index + 1),
      ]);
      if (found !== null) return found * e;
      return null;
    })
    .filter((item) => item !== null)
    .at(0);
}

function find(target: number, list: number[]) {
  if (list.length < 2) return null;
  const fst = list.at(0)!;
  const lst = list.at(-1)!;
  const result = fst + lst;
  if (result > target) return find(target, list.slice(0, -1));
  if (result < target) return find(target, list.slice(1));
  return fst * lst;
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
