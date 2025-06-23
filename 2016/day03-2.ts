function solution(content: string) {
  const lines = content
    .split("\n")
    .map((line) => [...line.matchAll(/\d+/g)].map(Number));

  const fst = lines.map((line) => line[0]);
  const snd = lines.map((line) => line[1]);
  const trd = lines.map((line) => line[2]);
  return search(fst) + search(snd) + search(trd);
}

function search(lst: number[]) {
  return lst.filter(
    (a, index, array) =>
      index % 3 === 0 && isTriangle(a, array[index + 1], array[index + 2])
  ).length;
}

function isTriangle(a: number, b: number, c: number) {
  return a + b > c && a + c > b && b + c > a;
}

Deno.readTextFile("day03.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
