function solution(content: string) {
  return content.split("\n").filter(isNice).length;
}

function isNice(line: string) {
  return [hasDoubledLetter, hasMoreThanThreeVowels, hasNoNaughty].every((f) =>
    f(line)
  );
}

function hasDoubledLetter(line: string) {
  return line
    .split("")
    .some((ch, index) => index > 0 && ch === line[index - 1]);
}

function hasMoreThanThreeVowels(line: string) {
  return line.split("").filter((ch) => "aeiou".includes(ch)).length >= 3;
}

function hasNoNaughty(line: string) {
  return ["ab", "cd", "pq", "xy"].every((naugthy) => !line.includes(naugthy));
}

Deno.readTextFile("day05.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
