import { encodeHex } from "@std/encoding/hex";
import { md5 } from "@takker/md5";

function solution(content: string) {
  let index = 1;
  while (true) {
    const hashed = md5(content + index);
    if (encodeHex(hashed).startsWith("000000")) {
      return index;
    }
    index += 1;
  }
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
