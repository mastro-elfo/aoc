function solution(content: string) {
  return content
    .matchAll(/mul\((\d+),(\d+)\)|(do\(\))|(don't\(\))/g)
    .reduce(
      ([active, value], cur): [boolean, number] => {
        const [_, a, b, isDo, isDont] = cur;
        if (isDo) return [true, value];
        if (isDont) return [false, value];
        if (active) return [true, value + Number(a) * Number(b)];
        return [false, value];
      },
      [true, 0] as [boolean, number]
    )
    .at(1);
}

Deno.readTextFile("day03.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
