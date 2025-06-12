function solution(content: string) {
  return content
    .split("\n")
    .map((line) => firstDigit(line, false) * 10 + firstDigit(line, true))
    .reduce((acc, cur) => acc + cur, 0);
}

function firstDigit(line: string, backward: boolean) {
  return parseInt(
    replace(line, backward)
      .split("")
      .find((ch) => "1234567890".includes(ch)) ?? "-1"
  );
}

function replace(line: string, backward: boolean) {
  if (backward) {
    return line
      .replace("twone", "1")
      .replaceAll("zerone", "1")
      .replaceAll("eightwo", "2")
      .replaceAll("eighthree", "3")
      .replaceAll("oneight", "8")
      .replaceAll("nineight", "8")
      .replaceAll("one", "1")
      .replaceAll("two", "2")
      .replaceAll("three", "3")
      .replaceAll("four", "4")
      .replaceAll("five", "5")
      .replaceAll("six", "6")
      .replaceAll("seven", "7")
      .replaceAll("eight", "8")
      .replaceAll("nine", "9")
      .replaceAll("zero", "0")
      .split("")
      .reverse()
      .join("");
  } else {
    return line
      .replaceAll("twone", "2")
      .replaceAll("zerone", "0")
      .replaceAll("eightwo", "8")
      .replaceAll("eighthree", "8")
      .replaceAll("oneight", "1")
      .replaceAll("nineight", "9")
      .replaceAll("one", "1")
      .replaceAll("two", "2")
      .replaceAll("three", "3")
      .replaceAll("four", "4")
      .replaceAll("five", "5")
      .replaceAll("six", "6")
      .replaceAll("seven", "7")
      .replaceAll("eight", "8")
      .replaceAll("nine", "9")
      .replaceAll("zero", "0");
  }
}

Deno.readTextFile("day01.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
