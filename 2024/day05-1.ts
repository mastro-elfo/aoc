type Rule = {
  before: number;
  after: number;
};

function solution(content: string) {
  const [fst, snd] = content.split("\n\n");
  const rules = parseRules(fst);
  const pages = parsePages(snd);
  return pages
    .filter((group) => allValid(rules, group))
    .map(middle)
    .reduce((acc, cur) => acc + cur, 0);
}

function middle(pages: number[]) {
  return pages.at(pages.length / 2)!;
}

function allValid(rules: Rule[], pages: number[]) {
  return rules.every((rule) => isValid(rule, pages));
}

function isValid(rule: Rule, pages: number[]) {
  const { before, after } = rule;
  if (!pages.includes(before) || !pages.includes(after)) return true;
  return pages.indexOf(before) < pages.indexOf(after);
}

function parseRules(content: string): Rule[] {
  return content.split("\n").map((line) => {
    const [before, after] = line.split("|").map(Number);
    return { before, after };
  });
}

function parsePages(content: string): number[][] {
  return content.split("\n").map((line) => line.split(",").map(Number));
}

Deno.readTextFile("day05.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
