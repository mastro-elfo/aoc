type Coords = {
  row: number;
  col: number;
};

function solution(content: string) {
  const matrix = content.split("\n").filter((line) => line);
  const xes = matrix.reduce(
    (rowAcc, row, rowIndex) => [
      ...rowAcc,
      ...row
        .split("")
        .reduce(
          (colAcc, col, colIndex) =>
            col === "X"
              ? [...colAcc, { col: colIndex, row: rowIndex }]
              : colAcc,
          [] as Coords[]
        ),
    ],
    [] as Coords[]
  );
  return xes.reduce((acc, cur) => acc + count(matrix, cur), 0);
}

function count(matrix: string[], { row, col }: Coords) {
  return [
    // Horizontal forward
    col < matrix[0].length - 3 &&
      matrix[row][col + 1] === "M" &&
      matrix[row][col + 2] === "A" &&
      matrix[row][col + 3] === "S",
    // Horizontal backward
    col > 2 &&
      matrix[row][col - 1] === "M" &&
      matrix[row][col - 2] === "A" &&
      matrix[row][col - 3] === "S",
    // Vertical Downward
    row < matrix.length - 3 &&
      matrix[row + 1][col] === "M" &&
      matrix[row + 2][col] === "A" &&
      matrix[row + 3][col] === "S",
    // Vertical Upward
    row > 2 &&
      matrix[row - 1][col] === "M" &&
      matrix[row - 2][col] === "A" &&
      matrix[row - 3][col] === "S",
    // Diagonal TL-BR
    row < matrix.length - 3 &&
      col < matrix[0].length - 3 &&
      matrix[row + 1][col + 1] === "M" &&
      matrix[row + 2][col + 2] === "A" &&
      matrix[row + 3][col + 3] === "S",
    // Diagonal BR-TL
    row > 2 &&
      col > 2 &&
      matrix[row - 1][col - 1] === "M" &&
      matrix[row - 2][col - 2] === "A" &&
      matrix[row - 3][col - 3] === "S",
    // Diagonal BL-TR
    row > 2 &&
      col < matrix[0].length - 3 &&
      matrix[row - 1][col + 1] === "M" &&
      matrix[row - 2][col + 2] === "A" &&
      matrix[row - 3][col + 3] === "S",
    //Diagonal TR-BL
    row < matrix.length - 3 &&
      col > 2 &&
      matrix[row + 1][col - 1] === "M" &&
      matrix[row + 2][col - 2] === "A" &&
      matrix[row + 3][col - 3] === "S",
  ].filter((item) => item).length;
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
