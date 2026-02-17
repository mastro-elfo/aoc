# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal

type Circuit = dict[str, int]
type Operation = Literal["AND", "OR", "XOR"]
type Gate = tuple[str, str, Operation, str]


def solution(content: str) -> Any:
    wires_block, gates_block, *_ = content.split("\n\n")
    circuit = parse_circuit(wires_block)
    gates = {parse_gate(line) for line in gates_block.split("\n")}
    zeta_wires = parse_zeta(wires_block, gates_block)
    while not all(zeta in circuit for zeta in zeta_wires):
        circuit = run(gates, circuit)
    return to_decimal(sort_circuit(circuit))


def sort_circuit(circuit: Circuit) -> list[int]:
    return [
        value
        for _, value in sorted(
            (item for item in circuit.items() if first(item).startswith("z")),
            key=first,
        )
    ]


def first[T](item: tuple[T, Any]) -> T:
    fst, _ = item
    return fst


def second[T](item: tuple[Any, T]) -> T:
    _, snd = item
    return snd


def to_decimal(lst: list[int]) -> int:
    if not lst:
        return 0
    fst, *rest = lst
    return fst + 2 * to_decimal(rest)


def run(gates: set[Gate], circuit: Circuit) -> Circuit:
    copy = circuit.copy()
    for l1, l2, op, l3 in gates:
        if l3 in copy:
            continue
        if l1 in copy and l2 in copy:
            copy[l3] = apply(copy[l1], copy[l2], op)
    return copy


def apply(v1: int, v2: int, op: Operation):
    if op == "AND":
        return v1 and v2
    if op == "OR":
        return v1 or v2
    if op == "XOR":
        return v1 != v2


def parse_zeta(wires_block: str, gates_block: str) -> set[str]:
    return {
        label for label, _ in (line.split(": ") for line in wires_block.split("\n"))
    } | {
        label
        for _, _, _, _, label in (line.split(" ") for line in gates_block.split("\n"))
    }


def parse_gate(line: str) -> Gate:
    l1, op, l2, _, l3, *_ = line.split(" ")
    if op in ("AND", "OR", "XOR"):
        return (l1, l2, op, l3)
    raise ValueError(f"Invalid operation: {op}")


def parse_circuit(block: str) -> Circuit:
    return {
        label: value
        for label, value in (parse_wire(line) for line in block.split("\n"))
    }


def parse_wire(line: str) -> tuple[str, int]:
    label, value, *_ = line.strip().split(": ")
    return label, int(value)


def main() -> None:
    with open("day24.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
