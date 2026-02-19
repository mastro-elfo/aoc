# pylint: disable=missing-module-docstring, missing-function-docstring


import functools
import re
from typing import Any, Literal

type Instruction = Literal["L", "R"]
type Tree = dict[str, tuple[str, str]]


def solution(content: str) -> Any:
    instructions = parse_instructions(content.split("\n")[0])
    tree = parse_tree(content.split("\n\n")[1])
    factor_list = [
        get_factors(period)
        for period in (
            get_period(instructions, tree, node)
            for node in (node for node in tree if node.endswith("A"))
        )
    ]

    common_factors: dict[int, int] = {}
    for factors in factor_list:
        for key in factors:
            common_factors[key] = max(common_factors.get(key, 0), factors[key])
    return functools.reduce(
        lambda acc, cur: acc * cur, [a**b for a, b in common_factors.items()], 1
    )


def get_factors(n: int) -> dict[int, int]:
    current = n
    output = {}
    prime_generator = primes()
    while current > 1:
        prime = next(prime_generator)
        while current % prime == 0:
            output[prime] = output.get(prime, 0) + 1
            current //= prime
    return output


def primes():
    yield 2
    lst = {2}
    current = 3
    while True:
        if all(current % n != 0 for n in lst):
            yield current
            lst.add(current)
        current += 2


def get_period(instructions: list[Instruction], tree: Tree, current: str) -> int:
    count = 0
    for instruction in generate_instructions(instructions):
        if current.endswith("Z"):
            return count
        left, right = tree[current]
        current = left if instruction == "L" else right
        count += 1

    return count


def generate_instructions(instructions: list[Instruction]):
    while True:
        for instruction in instructions:
            yield instruction


def parse_tree(block: str) -> Tree:
    tree = {}
    for line in block.split("\n"):
        match = re.match(r"(\w+) = \((\w+), (\w+)\)", line)
        if match is None:
            raise ValueError(f"Invalid line: {line}")
        tree[match[1]] = (match[2], match[3])
    return tree


def parse_instructions(line: str) -> list[Instruction]:
    return ["L" if char == "L" else "R" for char in line]


def main() -> None:
    with open("day08.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
