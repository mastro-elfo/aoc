type IPV7 = {
  nets: string[];
  hnets: string[];
};

function solution(content: string) {
  return content
    .split("\n")
    .map((line) => parse(line.trim()))
    .filter(supportTLS).length;
}

function supportTLS({ nets, hnets }: IPV7): boolean {
  return nets.some(hasABBA) && !hnets.some(hasABBA);
}

function hasABBA(part: string): boolean {
  if (part.length < 4) return false;
  return part
    .split("")
    .some(
      (p0, index, array) =>
        p0 === array[index + 3] &&
        array[index + 1] === array[index + 2] &&
        p0 !== array[index + 1]
    );
}

function parse(line: string): IPV7 {
  const nets: string[] = [];
  const hnets: string[] = [];
  let parseHnet: boolean = false;
  if (line[0] !== "[") nets.push("");
  line.split("").forEach((char) => {
    if (char === "[") {
      parseHnet = true;
      hnets.push("");
    } else if (char === "]") {
      parseHnet = false;
      nets.push("");
    } else if (parseHnet) {
      hnets[hnets.length - 1] += char;
    } else {
      nets[nets.length - 1] += char;
    }
  });
  return {
    nets,
    hnets,
  };
}

Deno.readTextFile("day07.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
