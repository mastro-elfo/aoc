# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal

type Operation = Literal["SUM"] | Literal["MUL"] | Literal["CNC"]

type Equation = tuple[int, list[int]]


def solution(content: list[str]) -> Any:
    return sum(get_result(x) for x in (parse(line) for line in content) if test(x))


def get_result(eq: Equation) -> int:
    result, _ = eq
    return result


def test(equation: Equation) -> bool:
    result, args = equation
    for ops in operations(len(args) - 1):
        if apply(args, ops) == result:
            return True
    return False


def apply(args: list[int], ops: list[Operation]) -> int:
    acc, *rest = args
    for val, op in zip(rest, ops):
        acc = calc(acc, val, op)
    return acc


def calc(x: int, y: int, op: Operation) -> int:
    if op == "MUL":
        return x * y
    if op == "SUM":
        return x + y
    if op == "CNC":
        return int(str(x) + str(y))


def operations(n: int):
    return [ter_to_op(n, ternary(x)) for x in range(3**n)]


def ter_to_op(n: int, seq: str) -> list[Operation]:
    return [
        "SUM" if x == "0" else "MUL" if x == "1" else "CNC" for x in seq.rjust(n, "0")
    ]


def ternary(n: int) -> str:
    if n == 0:
        return "0"
    val = n // 3
    rest = n % 3
    if val == 0:
        return str(rest)
    return ternary(n // 3) + str(rest)


def parse(line: str) -> Equation:
    fst, *rest = line.split(" ")
    return (int(fst[:-1]), [int(x) for x in rest])


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
