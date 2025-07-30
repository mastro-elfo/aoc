function solution(content: string) {
  return content.split("\n\n").filter(isValid).length;
}

function isValid(item: string) {
  return [
    isValidByr,
    isValidIyr,
    isValidEyr,
    isValidHgt,
    isValidHcl,
    isValidEcl,
    isValidPid,
  ].every((f) => f(item));
}

function isValidByr(item: string) {
  const match = /byr:(\d+)/.exec(item);
  if (!match) return false;
  const byr = Number(match.at(1));
  return 1920 <= byr && byr <= 2002;
}

function isValidIyr(item: string) {
  const match = /iyr:(\d+)/.exec(item);
  if (!match) return false;
  const iyr = Number(match.at(1));
  return 2010 <= iyr && iyr <= 2020;
}

function isValidEyr(item: string) {
  const match = /eyr:(\d+)/.exec(item);
  if (!match) return false;
  const eyr = Number(match.at(1));
  return 2020 <= eyr && eyr <= 2030;
}

function isValidHgt(item: string) {
  const match = /hgt:(\d+)(cm|in)/.exec(item);
  if (!match) return false;
  const hgt = Number(match.at(1));
  if (match.at(2) === "cm") return 150 <= hgt && hgt <= 193;

  return 59 <= hgt && hgt <= 76;
}

function isValidHcl(item: string) {
  return Boolean(/hcl:#[0-9a-f]{6}/.exec(item));
}

function isValidEcl(item: string) {
  return Boolean(/ecl:(amb|blu|brn|gry|grn|hzl|oth)/.exec(item));
}

function isValidPid(item: string) {
  const match = /pid:(\d+)/.exec(item);
  return Boolean(match?.at(1)?.length === 9);
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
