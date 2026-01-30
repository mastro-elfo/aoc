# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Vector = tuple[int, int]
type Machine = tuple[Vector, Vector, Vector]

OFFSET = 10_000_000_000_000


def solution(content: str) -> Any:
    return sum(cost(solve(parse(block))) for block in content.split("\n\n"))


def solve(machine: Machine) -> Vector:
    a, b, p = machine
    det_complete = det((a, b))
    if det_complete == 0:
        return (0, 0)
    det_a = det((p, b))
    det_b = det((a, p))
    sol_a = det_a % det_complete == 0
    sol_b = det_b % det_complete == 0
    if sol_a and sol_b:
        return (det_a // det_complete, det_b // det_complete)
    return (0, 0)


def cost(sol: Vector) -> int:
    a, b = sol
    return 3 * a + b


def det(matrix: tuple[tuple[int, int], tuple[int, int]]) -> int:
    (a, b), (c, d) = matrix
    return a * d - b * c


def parse(block: str) -> Machine:
    line_a, line_b, line_p = block.split("\n")
    match_a = re.match(r"Button A: X\+(\d+), Y\+(\d+)", line_a)
    match_b = re.match(r"Button B: X\+(\d+), Y\+(\d+)", line_b)
    match_p = re.match(r"Prize: X=(\d+), Y=(\d+)", line_p)
    if not match_a or not match_b or not match_p:
        raise ValueError(f"Invalid block: {block}")
    return (
        (int(match_a[1]), int(match_a[2])),
        (int(match_b[1]), int(match_b[2])),
        (int(match_p[1]) + OFFSET, int(match_p[2]) + OFFSET),
    )


def main() -> None:
    with open("day13.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
