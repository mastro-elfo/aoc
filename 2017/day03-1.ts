function solution(content: number) {
  if (content < 1) throw new Error(`Invalid input: ${content}`);
  if (content === 1) return 0;
  if (content < 10) return content % 2 === 0 ? 1 : 2;
  const sqrt = Math.ceil(Math.sqrt(content));
  const side = sqrt % 2 == 1 ? sqrt : sqrt + 1;
  const half = Math.floor(side / 2);
  const diff = Math.min(
    ...[
      Math.pow(side - 2, 2) + half,
      Math.pow(side - 2, 2) + half + side - 1,
      Math.pow(side - 2, 2) + half + (side - 1) * 2,
      Math.pow(side - 2, 2) + half + (side - 1) * 3,
    ].map((x) => Math.abs(x - content))
  );
  return half + diff;
}

console.log(solution(325489));
