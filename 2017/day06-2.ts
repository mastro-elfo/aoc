function solution(content: string) {
  let memory = content.split("\t").map(Number);
  const hashes: string[] = [];
  let last = String(memory);
  while (!hashes.includes(last)) {
    hashes.push(last);
    memory = reallocate(memory);
    last = String(memory);
  }
  return hashes.length - hashes.indexOf(last);
}

function reallocate(memory: number[]): number[] {
  const maxValue = Math.max(...memory);
  const index = memory.findIndex((v) => v === maxValue);
  const copy = memory.slice();
  copy[index] = 0;
  let idx = 0;
  while (idx < maxValue) {
    copy[(index + idx + 1) % copy.length] += 1;
    idx += 1;
  }
  return copy;
}

Deno.readTextFile("day06.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
