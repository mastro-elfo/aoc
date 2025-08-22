import { encodeHex } from "jsr:@std/encoding/hex";
import { md5 } from "jsr:@takker/md5";

function solution(content: string) {
  const g = generate(content);
  return Array.from({ length: 8 })
    .map((_) => g.next().value[5])
    .join("");
}

function* generate(prefix: string): Generator<string, string, unknown> {
  let counter = 0;
  while (true) {
    const hashed = encodeHex(md5(prefix + counter));
    if (hashed.startsWith("00000")) {
      yield hashed;
    }
    counter += 1;
  }
}

Deno.readTextFile("day05.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
