type Rule = {
  before: number;
  after: number;
};

function solution(content: string) {
  const [fst, snd] = content.split("\n\n");
  const rules = parseRules(fst);
  const pages = parsePages(snd);
  return pages
    .filter((group) => anyInvalid(rules, group))
    .map((group) => correct(group, rules))
    .map(middle)
    .reduce((acc, cur) => acc + cur, 0);
}

function correct(pages: number[], rules: Rule[]) {
  const copy = pages.slice();
  while (true) {
    const wrong = rules.find((rule) => isInvalid(rule, copy));
    if (wrong) {
      const { before, after } = wrong;
      const beforeIndex = copy.indexOf(before);
      const afterIndex = copy.indexOf(after);
      const temp = copy[beforeIndex];
      copy[beforeIndex] = copy[afterIndex];
      copy[afterIndex] = temp;
    } else {
      break;
    }
  }
  return copy;
}

function middle(pages: number[]) {
  return pages.at(pages.length / 2)!;
}

function anyInvalid(rules: Rule[], pages: number[]) {
  return rules.some((rule) => isInvalid(rule, pages));
}

function isInvalid(rule: Rule, pages: number[]) {
  const { before, after } = rule;
  if (!pages.includes(before) || !pages.includes(after)) return false;
  return pages.indexOf(before) > pages.indexOf(after);
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
