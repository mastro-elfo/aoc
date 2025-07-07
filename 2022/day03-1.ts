function solution(content: string) {
  return content
    .split("\n")
    .map((item) => priority(repeated(item)))
    .reduce((acc, cur) => acc + cur, 0);
}

function repeated(line: string) {
  const fst = line.slice(0, line.length / 2);
  const snd = line.slice(line.length / 2);
  return fst.split("").find((item) => snd.includes(item))!;
}

function priority(ch: string) {
  if (ch[0] >= "a" && ch[0] <= "z")
    return ch.charCodeAt(0) - "a".charCodeAt(0) + 1;
  if (ch[0] >= "A" && ch[0] <= "Z")
    return ch.charCodeAt(0) - "A".charCodeAt(0) + 27;
  return 0;
}

Deno.readTextFile("day03.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
