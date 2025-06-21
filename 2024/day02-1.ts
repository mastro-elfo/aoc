function solution(content: string) {
  return content
    .split("\n")
    .map((line) => line.split(" ").map(Number))
    .filter(isSafe).length;
}

function isSafe(level: number[]) {
  return isWithinRange(level) && (isDecreasing(level) || isIncreasing(level));
}

function isIncreasing(level: number[]) {
  return level.every((item, index, array) =>
    index < array.length - 1 ? item < array[index + 1] : true
  );
}

function isDecreasing(level: number[]) {
  return level.every((item, index, array) =>
    index < array.length - 1 ? item > array[index + 1] : true
  );
}

function isWithinRange(level: number[]) {
  return level.every((item, index, array) =>
    index < array.length - 1
      ? 1 <= Math.abs(item - array[index + 1]) &&
        Math.abs(item - array[index + 1]) <= 3
      : true
  );
}

Deno.readTextFile("day02.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
