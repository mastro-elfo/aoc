function solution(content: string[]) {
  const deltas = content.filter((item) => item).map(Number);
  let current = 0;
  const frequencies: number[] = [];
  const generator = repeat(deltas);
  while (true) {
    const delta = generator.next().value;
    if (frequencies.includes(current)) {
      return current;
    }
    frequencies.push(current);
    current += delta;
  }
}

function* repeat(deltas: number[]): Generator<number, number, unknown> {
  let index = 0;
  while (true) {
    yield deltas[index % deltas.length];
    index += 1;
  }
}

Deno.readTextFile("day01.dat")
  .then((content) => content.split("\n"))
  .then(solution)
  .then(console.log);
