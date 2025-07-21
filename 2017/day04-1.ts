function solution(content: string) {
  return content.split("\n").filter(isValid).length;
}

function isValid(line: string) {
  const words = line.trim().split(" ");
  return words.length === new Set(words).size;
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
