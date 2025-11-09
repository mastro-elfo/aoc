type IPV7 = {
  nets: string[];
  hnets: string[];
};

function solution(content: string) {
  return content
    .split("\n")
    .map((line) => parse(line.trim()))
    .filter(suppotSSL).length;
}

function suppotSSL({ nets, hnets }: IPV7): boolean {
  const abas = nets.map(getABAs).flat();
  return hnets.some((part) => abas.some((aba) => part.includes(getBAB(aba))));
}

function getBAB(aba: string) {
  return `${aba[1]}${aba[0]}${aba[1]}`;
}

function getABAs(part: string): string[] {
  return part
    .split("")
    .map((p0, index, array) =>
      index < array.length - 2
        ? `${p0}${array[index + 1]}${array[index + 2]}`
        : ""
    )
    .filter((item) => item && item[0] === item[2] && item[0] !== item[1]);
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
