function solution(content: string) {
  return countTrees(content.split("\n"), 0, 0, 3, 1);
}

function countTrees(
  treemap: string[],
  startX: number,
  startY: number,
  deltaX: number,
  deltaY: number
) {
  const width = treemap.at(0)?.length ?? 0;
  const height = treemap.length;
  let currentX = startX;
  let currentY = startY;
  let trees = 0;
  while (currentY < height) {
    if (treemap[currentY][currentX] === "#") trees += 1;
    currentX = (currentX + deltaX) % width;
    currentY += deltaY;
  }
  return trees;
}

Deno.readTextFile("day03.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
