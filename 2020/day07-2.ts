type Rule = {
  name: string;
  contains: { name: string; count: number }[];
};

function solution(content: string) {
  return count(content.split("\n").map(parse), "shiny gold");
}

const table: Record<string, number> = {};

function count(rules: Rule[], name: string): number {
  if (Object.keys(table).includes(name)) return table[name];
  const rule = rules.find((rule) => rule.name === name)!;
  const value = rule.contains
    .map((contains) => contains.count * (1 + count(rules, contains.name)))
    .reduce((acc, cur) => acc + cur, 0);
  table[name] = value;
  return value;
}

function parse(line: string): Rule {
  const match = line.match(/(.+) bags contain (.+)/);
  if (!match) throw new Error(`Invalid line: ${line}`);
  const [_, name, contains] = match;
  return {
    name,
    contains: parseContains(contains),
  };
}

function parseContains(line: string) {
  if (line.includes("no other bags")) return [];
  return line
    .split(",")
    .map((part) =>
      parsePart(
        part.replaceAll(" bags", "").replaceAll(" bag", "").replaceAll(".", "")
      )
    );
}

function parsePart(line: string) {
  const match = line.match(/(\d+) (.+)/);
  if (!match) throw new Error(`Invalid line: ${line}`);
  return { name: match[2], count: Number(match[1]) };
}

Deno.readTextFile("day07.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
