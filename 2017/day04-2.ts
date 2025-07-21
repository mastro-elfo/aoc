function solution(content: string) {
  return content.split("\n").filter(isValid).length;
}

function isValid(line: string) {
  const words = line.trim().split(" ");
  return (
    words.length === new Set(words).size &&
    words.every(
      (word) =>
        !isAnagram(
          word,
          words.filter((w) => w !== word)
        )
    )
  );
}

function isAnagram(word: string, others: string[]) {
  const srtd = word.split("").toSorted().join("");
  return others.some((w) => w.split("").toSorted().join("") === srtd);
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
