import { md5 } from "@takker/md5";
import { encodeHex } from "jsr:@std/encoding@1/hex";

function solution(content: string) {
  let index = 1;
  while (true) {
    const hashed = md5(content + index);
    if (encodeHex(hashed).startsWith("00000")) {
      return index;
    }
    index += 1;
  }
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
