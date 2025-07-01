function solution(content: string) {
  const treemap = content.split("\n");
  return (
    countTrees(treemap, 0, 0, 1, 1) *
    countTrees(treemap, 0, 0, 3, 1) *
    countTrees(treemap, 0, 0, 5, 1) *
    countTrees(treemap, 0, 0, 7, 1) *
    countTrees(treemap, 0, 0, 1, 2)
  );
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
