# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    program = [int(x) for x in content.split(",")]
    current = 0
    halt = False
    output = None
    while not halt:
        current, program, halt, n_output = execute(current, program)
        output = n_output if n_output is not None else output
    return output


def execute(current: int, program: list[int]):
    current_digits = str(program[current]).rjust(5, "0")
    fst_mod = current_digits[2]
    snd_mod = current_digits[1]
    opcode = current_digits[3] + current_digits[4]

    if opcode not in ["01", "02", "03", "04", "99"]:
        raise ValueError(f"Invalid opcode {opcode} at {current} in {program}")

    copy = program.copy()
    if opcode == "01":
        copy[copy[current + 3]] = get_value(program, current, 1, fst_mod) + get_value(
            program, current, 2, snd_mod
        )
        return (current + 4, copy, False, None)
    if opcode == "02":
        copy[copy[current + 3]] = get_value(program, current, 1, fst_mod) * get_value(
            program, current, 2, snd_mod
        )
        return (current + 4, copy, False, None)
    if opcode == "03":
        copy[copy[current + 1]] = 1
        return (current + 2, copy, False, None)
    if opcode == "04":
        copy[copy[current + 1]] = get_value(program, current, 1, fst_mod)
        return (current + 2, copy, False, copy[copy[current + 1]])

    # if opcode == "99":
    return (current, copy, True, None)


def get_value(program: list[int], current: int, param: int, mode: str):
    if mode == "1":
        return program[current + param]
    return program[program[current + param]]


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
