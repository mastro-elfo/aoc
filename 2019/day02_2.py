# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    program = [int(x) for x in content.split(",")]

    for noun in range(100):
        for verb in range(100):
            copy = program.copy()
            copy[1] = noun
            copy[2] = verb
            if execute(copy) == 19690720:
                return 100 * noun + verb


def execute(program: list[int]):
    current = 0
    halt = False

    while not halt:
        opcode = program[current]
        if opcode not in [1, 2, 99]:
            raise ValueError(f"Invalid opcode {opcode} at {current} in {program}")

        copy = program.copy()
        halt = False
        if opcode == 1:
            program[copy[current + 3]] = (
                copy[copy[current + 1]] + copy[copy[current + 2]]
            )
        if opcode == 2:
            program[copy[current + 3]] = (
                copy[copy[current + 1]] * copy[copy[current + 2]]
            )
        if opcode == 99:
            halt = True

        current += 4

    return program[0]


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
