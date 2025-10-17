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
  const root = lefts.find((left) => !rights.includes(left));
  if (root) return count(tree(root, edges));
}

function count(tr: Tree) {
  function helper(value: number, t: Tree): number {
    const { subtrees } = t;
    return (
      value + subtrees.reduce((acc, cur) => acc + helper(value + 1, cur), 0)
    );
  }
  return helper(0, tr);
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
