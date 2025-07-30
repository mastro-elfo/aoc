function solution(content: string) {
  return content
    .split("\n\n")
    .filter((item) =>
      ["byr:", "iyr:", "eyr:", "hgt:", "hcl:", "ecl:", "pid:"].every((key) =>
        item.includes(key)
      )
    ).length;
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
