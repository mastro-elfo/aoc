function solution(content: string) {
  return play(content.split("\n").map(Number));
}

function play(jumps: number[]) {
  let step = 0;
  let index = 0;
  const copy = jumps.slice();
  while (0 <= index && index < copy.length) {
    const offset = copy[index];
    copy[index] += offset < 3 ? 1 : -1;
    index += offset;
    step += 1;
  }
  return step;
}

Deno.readTextFile("day05.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
