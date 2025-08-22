import { encodeHex } from "jsr:@std/encoding/hex";
import { md5 } from "jsr:@takker/md5";

function solution(content: string) {
  const g = generate(content);
  const password = Array.from({ length: 8 }).map((_) => "");
  while (password.filter((it) => it).length < 8) {
    const hashed = g.next().value;
    const index = parseInt(hashed[5], 16);
    if (index > 7) {
      continue;
    }
    if (password[index]) {
      continue;
    }
    password[index] = hashed[6];
  }
  return password.join("");
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
