type Cell = { num: number; check: boolean };
type Board = Cell[][];

function solution(content: string) {
  const lines = content.split("\n");
  const numbers = lines[0].split(",").map(Number);
  const boards = parseBoards(lines.slice(2).join("\n").split("\n\n"));
  const [winningBoard, winningNumber] = play(boards, numbers);
  return (
    winningNumber *
    winningBoard
      .flat()
      .filter(({ check }) => !check)
      .map(({ num }) => num)
      .reduce((acc, cur) => acc + cur, 0)
  );
}

function isWinningLine(line: Cell[]) {
  return line.every(({ check }) => check);
}

function transpose(board: Board) {
  return board.map((_, index) => board.map((row) => row[index]));
}

function isWinner(board: Board) {
  return board.some(isWinningLine) || transpose(board).some(isWinningLine);
}

function winner(boards: Board[]) {
  for (const board of boards) {
    if (isWinner(board)) {
      return board;
    }
  }
  return null;
}

function play(boards: Board[], numbers: number[]): [Board, number] {
  let currentBoards = boards.slice();
  for (const anumber of numbers) {
    currentBoards = currentBoards.map((board) =>
      board.map((row) =>
        row.map(({ num, check }) => ({ num, check: check || num === anumber }))
      )
    );
    const winningBoard = winner(currentBoards);
    if (winningBoard) {
      return [winningBoard, anumber];
    }
  }
  return [[], 0];
}

function parseBoards(blocks: string[]) {
  return blocks.map((block) =>
    block.split("\n").map((line) =>
      line
        .split(" ")
        .filter((item) => item)
        .map((item) => ({ check: false, num: Number(item) }))
    )
  );
}

Deno.readTextFile("day04.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
