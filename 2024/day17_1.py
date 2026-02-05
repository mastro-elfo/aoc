# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Registers = tuple[int, int, int]
type Program = list[int]
type State = tuple[int, Registers, list[int], bool]


def solution(content: str) -> Any:
    reg_block, prog_block = content.split("\n\n")
    registers: Registers = parse_registers(reg_block)
    program: Program = parse_program(prog_block)
    _, _, output, _ = exec_program(program, (0, registers, [], False))
    return ",".join(str(x) for x in output)


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


def parse_registers(block: str) -> Registers:
    reg_a, reg_b, reg_c = [
        int(x) for _, x in (line.split(": ") for line in block.split("\n"))
    ]

    return (reg_a, reg_b, reg_c)


def main() -> None:
    with open("day17.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
