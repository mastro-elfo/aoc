# pylint: disable=missing-module-docstring, missing-function-docstring


from functools import lru_cache
from typing import Any, Literal

type Instruction = tuple[
    str,
    str,
    Literal["SIG"]
    | Literal["NOT"]
    | Literal["AND"]
    | Literal["OR"]
    | Literal["LSHIFT"]
    | Literal["RSHIFT"],
    str,
]


instructions: list[Instruction] = []


def solution(content: list[str]) -> Any:
    # Using global statement to enable cache on `evaluate`
    global instructions  # pylint: disable=global-statement
    instructions = [parse(line) for line in content]
    return evaluate(target("a"))


@lru_cache
def evaluate(i: Instruction):
    left, right, op, _ = i
    if op == "SIG" and is_number(left):
        return int(left)
    if op == "SIG":
        return evaluate(target(left))
    if op == "NOT" and is_number(left):
        return norm(~int(left))
    if op == "NOT":
        return norm(~evaluate(target(left)))
    if op == "AND" and is_number(left) and is_number(right):
        return int(left) & int(right)
    if op == "AND" and is_number(left):
        return int(left) & evaluate(target(right))
    if op == "AND" and is_number(right):
        return int(right) & evaluate(target(left))
    if op == "AND":
        return evaluate(target(left)) & evaluate(target(right))
    if op == "OR" and is_number(left) and is_number(right):
        return int(left) | int(right)
    if op == "OR" and is_number(left):
        return int(left) | evaluate(target(right))
    if op == "OR" and is_number(right):
        return int(right) | evaluate(target(left))
    if op == "OR":
        return evaluate(target(left)) | evaluate(target(right))
    if op == "LSHIFT" and is_number(left):
        return int(left) << int(right)
    if op == "LSHIFT":
        return evaluate(target(left)) << int(right)
    if op == "RSHIFT" and is_number(left):
        return int(left) >> int(right)
    if op == "RSHIFT":
        return evaluate(target(left)) >> int(right)
    raise ValueError(f"Invalid operation {op}")


def norm(val: int):
    if val < 0:
        return 65536 + val
    return val


def target_of(i: Instruction):
    return i[3]


def target(trg: str) -> Instruction:
    return [t for t in instructions if target_of(t) == trg][0]


def parse(line: str) -> Instruction:
    parts = line.strip().split(" ")
    if "NOT" in parts:
        return (parts[1], "", "NOT", parts[-1])
    if "AND" in parts:
        return (parts[0], parts[2], "AND", parts[-1])
    if "OR" in parts:
        return (parts[0], parts[2], "OR", parts[-1])
    if "LSHIFT" in parts:
        return (parts[0], parts[2], "LSHIFT", parts[-1])
    if "RSHIFT" in parts:
        return (parts[0], parts[2], "RSHIFT", parts[-1])
    return (parts[0], "", "SIG", parts[-1])


def is_number(val: str):
    return all(ch in "1234567890" for ch in val)


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
