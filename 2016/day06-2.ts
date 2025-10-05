type Frequencies = Record<string, number>[];

function solution(content: string) {
  const lines = content.split("\n");
  const frequencies: Frequencies = Array.from({ length: lines[0].length }).map(
    (_) => ({})
  );
  lines.forEach((line) => {
    line.split("").forEach((char, index) => {
      frequencies[index][char] = (frequencies[index][char] ?? 0) + 1;
    });
  });
  return frequencies
    .map(
      (f) =>
        Object.entries(f).toSorted(
          ([_a, aFreq], [_b, bFreq]) => aFreq - bFreq
        )[0][0]
    )
    .join("");
}

Deno.readTextFile("day06.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
