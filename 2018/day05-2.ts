function solution(content: string) {
  return Math.min(
    ...Array.from(new Set(content.toLowerCase()))
      .map((ch) => content.split("").filter((it) => it.toLowerCase() !== ch))
      .map(react)
      .map((result) => result.length)
  );
}

function react(polymer: string[]) {
  const copy = polymer.slice();
  let isOn = true;

  while (isOn) {
    isOn = false;
    for (let index = 0; index < copy.length; index++) {
      if (
        index < copy.length - 1 &&
        copy[index].toLowerCase() === copy[index + 1].toLowerCase() &&
        copy[index] !== copy[index + 1]
      ) {
        isOn = true;
        copy.splice(index, 2);
        break;
      }
    }
  }

  return copy;
}

Deno.readTextFile("day05.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
