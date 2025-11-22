type Rule = {
  name: string;
  contains: string[];
};

function solution(content: string) {
  const rules = content.split("\n").map(parse);
  return rules.filter((rule) => isValid(rules, rule)).length;
}

const table: Record<string, boolean> = {};

function isValid(rules: Rule[], rule: Rule): boolean {
  const { name, contains } = rule;
  if (Object.keys(table).includes(name)) {
    return table[name];
  }
  const result =
    contains.includes("shiny gold") ||
    rules
      .filter(({ name }) => contains.includes(name))
      .some((rule) => isValid(rules, rule));
  table[name] = result;
  return result;
}

function parse(line: string): Rule {
  const match = line.match(/(.+) bags contain (.+)/);
  if (!match) throw new Error(`Invalid line: ${line}`);
  const [_, name, contains] = match;
  if (contains.includes("no other bags")) return { name, contains: [] };
  return {
    name,
    contains: contains
      .replaceAll(" bags", "")
      .replaceAll(" bag", "")
      .replaceAll(".", "")
      .replaceAll(/\d/g, "")
      .split(",")
      .map((color) => color.trim()),
  };
}

Deno.readTextFile("day07.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
