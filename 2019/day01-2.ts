function solution(content: string) {
  return content
    .split("\n")
    .map((item) => getFuel(parseInt(item)))
    .reduce((acc, cur) => acc + cur, 0);
}

function getFuel(mass: number): number {
  const fuel = Math.floor(mass / 3) - 2;
  if (fuel <= 0) return 0;
  return fuel + getFuel(fuel);
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
