# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    program = [int(x) for x in content.split(",")]
    program[1] = 12
    program[2] = 2
    current = 0
    halt = False
    while not halt:
        current, program, halt = execute(current, program)
    return program[0]


def execute(current: int, program: list[int]):
    opcode = program[current]
    if opcode not in [1, 2, 99]:
        raise ValueError(f"Invalid opcode {opcode} at {current} in {program}")

    copy = program.copy()
    halt = False
    if opcode == 1:
        copy[copy[current + 3]] = copy[copy[current + 1]] + copy[copy[current + 2]]
    if opcode == 2:
        copy[copy[current + 3]] = copy[copy[current + 1]] * copy[copy[current + 2]]
    if opcode == 99:
        halt = True

    return (current + 4, copy, halt)


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
