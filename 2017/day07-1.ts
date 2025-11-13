type Program = {
  name: string;
  programs: string[];
};

function solution(content: string) {
  const programs = content.split("\n").map(parse);
  const names = programs.map(({ name }) => name);
  const topNames = new Set(programs.map(({ programs }) => programs).flat());
  return names.find((name) => !topNames.has(name));
}

function parse(line: string): Program {
  const match = line.match(/(\w+) \(\d+\)( -> ([\w\s,]+))?/);
  if (!match) {
    throw new Error(`Invalid line: ${line}`);
  }
  return {
    name: match[1],
    programs: match[3] ? match[3].trim().split(", ") : [],
  };
}

Deno.readTextFile("day07.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
