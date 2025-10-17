type Edge = {
  from: string;
  to: string;
};

type Tree = {
  label: string;
  subtrees: Tree[];
};

function solution(content: string) {
  const edges = content.split("\n").map(parse);
  const lefts = edges.map(({ from }) => from);
  const rights = edges.map(({ to }) => to);
  const root = lefts.find((left) => !rights.includes(left))!;
  const aTree = tree(root, edges);
  const youBranch = branch("YOU", aTree);
  const sanBranch = branch("SAN", aTree);
  const index = youBranch.findIndex((label, idx) => label !== sanBranch[idx]);
  return youBranch.length + sanBranch.length - 2 * index - 2;
}

function branch(node: string, tr: Tree): string[] {
  const { label, subtrees } = tr;
  if (node === label) return [label];
  const sbt = subtrees.map((t) => branch(node, t));
  if (sbt.some((st) => st.length))
    return [label].concat(sbt.find((st) => st.length)!);
  return [];
}

function tree(label: string, edges: Edge[]): Tree {
  return {
    label,
    subtrees: edges
      .filter(({ from }) => from === label)
      .map(({ to }) => tree(to, edges)),
  };
}

function parse(line: string): Edge {
  const parts = line.split(")");
  return {
    from: parts[0],
    to: parts[1],
  };
}

Deno.readTextFile("day06.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
