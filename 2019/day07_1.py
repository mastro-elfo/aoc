# pylint: disable=missing-module-docstring, missing-function-docstring


from itertools import permutations
from typing import Any

type State = tuple[list[int], int, list[int], bool, int]


def solution(content: str) -> Any:
    program = [int(x) for x in content.split(",")]
    phases = permutations([0, 1, 2, 3, 4], 5)
    return max(amplify(program, setting) for setting in phases)


def amplify(program: list[int], setting: tuple[int, int, int, int, int]):
    inpt = 0
    for s in setting:
        inpt = run(program, [s, inpt])
    return inpt


def run(program: list[int], inputs: list[int]):
    state: State = (inputs, 0, program.copy(), False, 0)
    _, _, _, halt, output = state
    while not halt:
        state = execute(state)
        _, _, _, halt, output = state
    return output


def execute(state: State) -> State:
    inputs, current, program, _, output = state
    digits = str(program[current]).rjust(5, "0")
    fst_mod = digits[2]
    snd_mod = digits[1]
    opcode = digits[3] + digits[4]

    if opcode not in ["01", "02", "03", "04", "05", "06", "07", "08", "99"]:
        raise ValueError(f"Invalid opcode {opcode} at {current}")

    copy = program.copy()

    if opcode == "01":
        copy[copy[current + 3]] = get_value(program, current, 1, fst_mod) + get_value(
            program, current, 2, snd_mod
        )
        return (inputs, current + 4, copy, False, 0)
    if opcode == "02":
        copy[copy[current + 3]] = get_value(program, current, 1, fst_mod) * get_value(
            program, current, 2, snd_mod
        )
        return (inputs, current + 4, copy, False, 0)
    if opcode == "03":
        copy[copy[current + 1]] = inputs[0]
        return (inputs[1:], current + 2, copy, False, 0)
    if opcode == "04":
        copy[copy[current + 1]] = get_value(program, current, 1, fst_mod)
        return (inputs, current + 2, copy, False, copy[copy[current + 1]])
    if opcode == "05":
        if get_value(program, current, 1, fst_mod) != 0:
            return (inputs, get_value(program, current, 2, snd_mod), copy, False, 0)
        return (inputs, current + 3, copy, False, 0)
    if opcode == "06":
        if get_value(program, current, 1, fst_mod) == 0:
            return (inputs, get_value(program, current, 2, snd_mod), copy, False, 0)
        return (inputs, current + 3, copy, False, 0)
    if opcode == "07":
        copy[copy[current + 3]] = (
            1
            if get_value(program, current, 1, fst_mod)
            < get_value(program, current, 2, snd_mod)
            else 0
        )
        return (inputs, current + 4, copy, False, 0)
    if opcode == "08":
        copy[copy[current + 3]] = (
            1
            if get_value(program, current, 1, fst_mod)
            == get_value(program, current, 2, snd_mod)
            else 0
        )
        return (inputs, current + 4, copy, False, 0)

    return (inputs, current, copy, True, output)


def get_value(program: list[int], current: int, param: int, mode: str):
    if mode == "1":
        return program[current + param]
    return program[program[current + param]]


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
