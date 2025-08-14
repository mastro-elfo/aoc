type Coords = {
  row: number;
  col: number;
};

function solution(content: string) {
  return content
    .split("\n")
    .filter((line) => line)
    .reduce(
      (rowAcc, row, rowIndex, matrix) => [
        ...rowAcc,
        ...row
          .split("")
          .reduce(
            (colAcc, col, colIndex) =>
              col === "A" && check(matrix, rowIndex, colIndex)
                ? [...colAcc, true]
                : colAcc,
            [] as boolean[]
          ),
      ],
      [] as boolean[]
    ).length;
}

function check(matrix: string[], row: number, col: number) {
  if (
    row === 0 ||
    row === matrix.length - 1 ||
    col === 0 ||
    col === matrix[0].length - 1
  )
    return false;
  return (
    (matrix[row - 1][col - 1] === "M" &&
      matrix[row + 1][col + 1] === "S" &&
      matrix[row + 1][col - 1] === "M" &&
      matrix[row - 1][col + 1] === "S") ||
    (matrix[row - 1][col - 1] === "M" &&
      matrix[row + 1][col + 1] === "S" &&
      matrix[row - 1][col + 1] === "M" &&
      matrix[row + 1][col - 1] === "S") ||
    (matrix[row + 1][col + 1] === "M" &&
      matrix[row - 1][col - 1] === "S" &&
      matrix[row - 1][col + 1] === "M" &&
      matrix[row + 1][col - 1] === "S") ||
    (matrix[row + 1][col + 1] === "M" &&
      matrix[row - 1][col - 1] === "S" &&
      matrix[row + 1][col - 1] === "M" &&
      matrix[row - 1][col + 1] === "S")
  );
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
