function solution(content: string) {
  const positions = content.split(",").map(Number);
  const minPosition = Math.min(...positions);
  const maxPosition = Math.max(...positions);
  return Math.min(
    ...Array.from({ length: maxPosition - minPosition })
      .map((_, index) => minPosition + index)
      .map((end) => fuelTot(positions, end))
  );
}

function fuelTo(start: number, end: number): number {
  return gauss(Math.abs(start - end));
}

function fuelTot(starts: number[], end: number): number {
  return starts
    .map((start) => fuelTo(start, end))
    .reduce((acc, cur) => acc + cur, 0);
}

function gauss(n: number): number {
  return (n * (n + 1)) / 2;
}

Deno.readTextFile("day07.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
