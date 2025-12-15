# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Generator, Literal

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


def operations(n: int) -> Generator[list[Operation], None, None]:
    if n == 0:
        yield []
    else:
        for rest in operations(n - 1):
            yield ["CNC", *rest]
            yield ["MUL", *rest]
            yield ["SUM", *rest]


def parse(line: str) -> Equation:
    fst, *rest = line.split(" ")
    return (int(fst[:-1]), [int(x) for x in rest])


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
