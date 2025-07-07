function solution(content: string) {
  return groupBy(content.split("\n"))
    .map((item) => priority(badge(item)))
    .reduce((acc, cur) => acc + cur, 0);
}

function badge(group: string[]) {
  const [fst, snd, trd] = group;
  return fst
    .split("")
    .find((item) => snd.includes(item) && trd.includes(item))!;
}

function groupBy(lines: string[]) {
  return lines.reduce((acc, cur) => {
    const last = acc.at(-1);
    if (!last) return [[cur]];
    if (last.length === 3) return [...acc, [cur]];
    return [...acc.slice(0, -1), [...last, cur]];
  }, [] as string[][]);
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
