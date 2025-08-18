function solution(content: string) {
  return content.split("\n").filter(isNice).length;
}

function isNice(line: string) {
  return [hasRepeatingDouble, hasEfeRule].every((f) => f(line));
}

function hasRepeatingDouble(line: string) {
  if (line.length < 2) return false;
  const isInfixOf = line.slice(2).includes(line.slice(0, 2));
  if (isInfixOf) return true;
  return hasRepeatingDouble(line.slice(1));
}

function hasEfeRule(line: string) {
  if (line.length < 3) return false;
  const isEfe = line[0] === line[2];
  if (isEfe) return true;
  return hasEfeRule(line.slice(1));
}

Deno.readTextFile("day05.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
