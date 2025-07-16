type Room = {
  name: string;
  code: number;
  checksum: string;
};

function solution(content: string) {
  return content
    .split("\n")
    .map(parse)
    .filter((room) => checksum(room.name) === room.checksum)
    .map((room) => ({
      ...room,
      name: decrypt(room.name, room.code),
    }))
    .filter((room) => room.name.includes("north"))
    .at(0)?.code;
}

function decrypt(name: string, key: number) {
  return name
    .split("")
    .map((char) => rotate(char, key))
    .join("");
}

function rotate(char: string, key: number) {
  if (char === "-") return " ";
  return String.fromCharCode(
    "a".charCodeAt(0) + ((char.charCodeAt(0) - "a".charCodeAt(0) + key) % 26)
  );
}

function checksum(name: string) {
  function helper(count: Record<string, number>, line: string) {
    if (line.length === 0) {
      return Object.entries(count)
        .toSorted(
          ([charA], [charB]) => charA.charCodeAt(0) - charB.charCodeAt(0)
        )
        .toSorted(([_A, countA], [_B, countB]) => countB - countA)
        .map(([char]) => char)
        .join("")
        .slice(0, 5);
    }
    const char = line.at(0)!;
    const rest = line.slice(1);
    count[char] = 1 + (count[char] ?? 0);
    return helper(count, rest);
  }
  return helper({}, name.replaceAll("-", ""));
}

function parse(line: string): Room {
  const match = line.match(/([a-z\-]+)(\d+)\[([a-z]+)\]/);
  if (!match) throw new Error(`Invalid line: ${line}`);
  return {
    checksum: match.at(3)!,
    code: Number(match.at(2)),
    name: match.at(1)!,
  };
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
