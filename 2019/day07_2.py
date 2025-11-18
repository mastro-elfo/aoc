# pylint: disable=missing-module-docstring, missing-function-docstring


from itertools import permutations
from typing import Any

type State = tuple[list[int], int, list[int], bool, bool, int]


def solution(content: str) -> Any:
    program = [int(x) for x in content.split(",")]
    phases = permutations([5, 6, 7, 8, 9], 5)
    return max(amplify(program, setting) for setting in phases)


def amplify(program: list[int], setting: tuple[int, int, int, int, int]):
    pa, pb, pc, pd, pe = setting

    state_a = ([pa, 0], 0, program.copy(), False, False, 0)
    state_a = run(state_a)
    state_b = ([pb, get_output(state_a)], 0, program.copy(), False, False, 0)
    state_b = run(state_b)
    state_c = ([pc, get_output(state_b)], 0, program.copy(), False, False, 0)
    state_c = run(state_c)
    state_d = ([pd, get_output(state_c)], 0, program.copy(), False, False, 0)
    state_d = run(state_d)
    state_e = ([pe, get_output(state_d)], 0, program.copy(), False, False, 0)
    state_e = run(state_e)

    while any(
        not halt
        for _, _, _, _, halt, _ in [state_a, state_b, state_c, state_d, state_e]
    ):
        state_a = run(reset(state_a, [get_output(state_e)]))
        state_b = run(reset(state_b, [get_output(state_a)]))
        state_c = run(reset(state_c, [get_output(state_b)]))
        state_d = run(reset(state_d, [get_output(state_c)]))
        state_e = run(reset(state_e, [get_output(state_d)]))

    return get_output(state_e)


def get_output(state: State) -> int:
    _, _, _, _, _, output = state
    return output


def reset(state: State, inputs: list[int]) -> State:
    _, current, program, _, halt, output = state
    return (inputs, current, program, False, halt, output)


def run(state: State):
    _, _, _, suspend, halt, _ = state
    if halt:
        return state
    while not suspend:
        state = execute(state)
        _, _, _, suspend, _, _ = state
    return state


def execute(state: State) -> State:
    inputs, current, program, _, _, output = state
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
        return (inputs, current + 4, copy, False, False, output)
    if opcode == "02":
        copy[copy[current + 3]] = get_value(program, current, 1, fst_mod) * get_value(
            program, current, 2, snd_mod
        )
        return (inputs, current + 4, copy, False, False, output)
    if opcode == "03":
        copy[copy[current + 1]] = inputs[0]
        return (
            inputs if len(inputs) == 1 else inputs[1:],
            current + 2,
            copy,
            False,
            False,
            output,
        )
    if opcode == "04":
        copy[copy[current + 1]] = get_value(program, current, 1, fst_mod)
        return (inputs, current + 2, copy, True, False, copy[copy[current + 1]])
    if opcode == "05":
        if get_value(program, current, 1, fst_mod) != 0:
            return (
                inputs,
                get_value(program, current, 2, snd_mod),
                copy,
                False,
                False,
                0,
            )
        return (inputs, current + 3, copy, False, False, output)
    if opcode == "06":
        if get_value(program, current, 1, fst_mod) == 0:
            return (
                inputs,
                get_value(program, current, 2, snd_mod),
                copy,
                False,
                False,
                0,
            )
        return (inputs, current + 3, copy, False, False, output)
    if opcode == "07":
        copy[copy[current + 3]] = (
            1
            if get_value(program, current, 1, fst_mod)
            < get_value(program, current, 2, snd_mod)
            else 0
        )
        return (inputs, current + 4, copy, False, False, output)
    if opcode == "08":
        copy[copy[current + 3]] = (
            1
            if get_value(program, current, 1, fst_mod)
            == get_value(program, current, 2, snd_mod)
            else 0
        )
        return (inputs, current + 4, copy, False, False, output)

    return (inputs, current, copy, True, True, output)


def get_value(program: list[int], current: int, param: int, mode: str):
    if mode == "1":
        return program[current + param]
    return program[program[current + param]]


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
