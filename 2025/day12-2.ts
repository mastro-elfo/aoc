function solution(_: string) {
  return "Nothing to do";
}

Deno.readTextFile("day12.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
