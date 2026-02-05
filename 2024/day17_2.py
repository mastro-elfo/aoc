# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Program = list[int]
type Registers = tuple[int, int, int]
type State = tuple[int, Registers, list[int], bool]


def solution(content: str) -> Any:
    _, prog_block = content.split("\n\n")
    return reverse(parse_program(prog_block))


def reverse(program: Program) -> int:
    def helper(program: Program, index: int, carry: int) -> int | None:
        if index == len(program):
            return carry
        regs = ((carry << 3) + reg for reg in range(8))
        candidates = (
            (reg, exec_program(program, (0, (reg, 0, 0), [], False))) for reg in regs
        )
        candidates = (
            (reg, output)
            for reg, (_, _, output, _) in candidates
            if len(output) == index + 1
        )
        for reg, output in candidates:
            if all(x == y for x, y in zip(program[::-1], output[::-1])):
                recursive = helper(program, index + 1, reg)
                if recursive is not None:
                    return recursive
        return None

    sol = helper(program, 0, 0)
    if sol is None:
        raise ValueError(f"No solution found for {program}")
    return sol


def exec_program(program: Program, state: State) -> State:
    halt = False
    while not halt:
        state = exec_instruction(program, state)
        _, _, _, halt = state
    return state


def exec_instruction(program: Program, state: State) -> State:
    current, registers, output, _ = state
    reg_a, reg_b, reg_c = registers

    if current >= len(program):
        return current, (reg_a, reg_b, reg_c), output, True

    cmd = program[current]
    opc = program[current + 1]

    if cmd == 0:
        return (
            current + 2,
            (reg_a // (2 ** get_combo(opc, registers)), reg_b, reg_c),
            output,
            False,
        )
    if cmd == 1:
        return (current + 2, (reg_a, reg_b ^ opc, reg_c), output, False)
    if cmd == 2:
        return (
            current + 2,
            (reg_a, get_combo(opc, registers) & 0x7, reg_c),
            output,
            False,
        )
    if cmd == 3 and reg_a == 0:
        return (current + 2, registers, output, False)
    if cmd == 3:
        return (opc, registers, output, False)
    if cmd == 4:
        return (current + 2, (reg_a, reg_b ^ reg_c, reg_c), output, False)
    if cmd == 5:
        return (
            current + 2,
            registers,
            output + [get_combo(opc, registers) & 0x7],
            False,
        )
    if cmd == 6:
        return (
            current + 2,
            (reg_a, reg_a // (2 ** get_combo(opc, registers)), reg_c),
            output,
            False,
        )
    if cmd == 7:
        return (
            current + 2,
            (reg_a, reg_b, reg_a // (2 ** get_combo(opc, registers))),
            output,
            False,
        )
    raise ValueError(f"Invalid command: {cmd}")


def get_combo(opc: int, registers: Registers) -> int:
    reg_a, reg_b, reg_c = registers
    if opc < 4:
        return opc
    if opc == 4:
        return reg_a
    if opc == 5:
        return reg_b
    if opc == 6:
        return reg_c
    raise ValueError(f"Invalid operand: {opc}")


def parse_program(block: str) -> Program:
    return [int(x) for x in block[9:].split(",")]


def main() -> None:
    with open("day17.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
