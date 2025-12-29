type File = {
  name: string[];
  size: number;
  type: "F" | "D";
};

type State = {
  current: string[];
  files: File[];
};

function solution(content: string) {
  const files = parse(content.split("\n"));
  return files
    .filter((file) => file.type === "D")
    .map((item) => size(files, item))
    .filter((value) => value < 100_000)
    .reduce((acc, cur) => acc + cur, 0);
}

function contains(f1: File, f2: File): boolean {
  return f2.name.join("").endsWith(f1.name.join(""));
}

function size(files: File[], file: File): number {
  return files
    .filter((item) => contains(file, item))
    .reduce((acc, item) => item.size + acc, 0);
}

function parse(content: string[]): File[] {
  return content.reduce(parseLine, { current: [], files: [] }).files;
}

function parseLine(state: State, line: string): State {
  const clean = line.trim();
  if (clean.startsWith("$ ls")) return state;
  const { current, files } = state;
  if (clean.startsWith("$ cd ..")) return { current: current.slice(1), files };
  if (clean.startsWith("$ cd "))
    return { current: [clean.slice(5), ...current], files };
  if (clean.startsWith("dir "))
    return {
      current,
      files: [
        { name: [clean.slice(4), ...current], size: 0, type: "D" },
        ...files,
      ],
    };
  const [fsize, fname] = clean.split(" ");
  return {
    current,
    files: [
      { name: [fname, ...current], size: Number(fsize), type: "F" },
      ...files,
    ],
  };
}

Deno.readTextFile("day07.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
