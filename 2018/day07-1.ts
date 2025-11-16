type Step = { name: string; requires: string[] };

function solution(content: string) {
  const lines = content.split("\n");
  const names = new Set(
    lines
      .map((line) => line.split(" ")[1])
      .concat(lines.map((line) => line.split(" ").at(-3)!))
  );
  const steps: Step[] = Array.from(names).map((name) => ({
    name,
    requires: lines
      .map((line) => line.split(" "))
      .filter((items) => items.at(-3) === name)
      .map((items) => items[1]),
  }));
  return unwind(steps);
}

function unwind(steps: Step[]) {
  let copy = steps.slice();
  let output = "";
  while (copy.length) {
    const [first, ..._rest] = copy
      .filter((step) => !step.requires.length)
      .map((step) => step.name)
      .toSorted();
    output += first;
    copy = copy.filter((step) => step.name !== first);
    copy = copy.map(({ name, requires }) => ({
      name,
      requires: requires.filter((req) => req !== first),
    }));
  }
  return output;
}

Deno.readTextFile("day07.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
