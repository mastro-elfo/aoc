function solution(content: string) {
  return find(
    content
      .split("\n")
      .map((item) => parseInt(item))
      .toSorted()
  );
}

function find(list: number[]) {
  const fst = list.at(0)!;
  const lst = list.at(-1)!;
  const result = fst + lst;
  if (result > 2020) return find(list.slice(0, -1));
  if (result < 2020) return find(list.slice(1));
  return fst * lst;
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
