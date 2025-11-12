# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Program = tuple[str, int, list[str]]


def solution(content: list[str]) -> Any:
    programs = [parse(line) for line in content]

    unblncd = unbalanced(programs)
    wghts = weights(programs, unblncd)
    hist = histogram(wghts)

    min_key = [key for key, val in hist.items() if val == 1][0]
    oth_key = [key for key, val in hist.items() if val != 1][0]
    min_index = wghts.index(min_key)

    return get(programs, unblncd[2][min_index])[1] + oth_key - min_key


def wrong(programs: list[Program], names: list[str], wght: int) -> Program:
    print([prog for prog in programs if prog[0] in names])
    return [prog for prog in programs if prog[0] in names and prog[1] == wght][0]


def unbalanced(programs: list[Program]) -> Program:
    unblncd = [prog for prog in programs if is_unbalanced(weights(programs, prog))]
    length = len(unblncd)
    while len(unblncd) > 1:
        names = [name for name, _, _ in unblncd]
        top_names = set(name for _, _, progs in unblncd for name in progs)
        bottom_names = [name for name in names if name not in top_names]
        unblncd = [prog for prog in unblncd if prog[0] not in bottom_names]
        if len(unblncd) == length:
            raise RecursionError("Infinite loop detected")
        length = len(unblncd)
    return unblncd[0]


def histogram(nums: list[int]) -> dict[int, int]:
    hist: dict[int, int] = {}
    for num in nums:
        hist[num] = hist.get(num, 0) + 1
    return hist


def is_unbalanced(nums: list[int]) -> bool:
    if not nums:
        return False
    first, *rest = nums
    return any(num != first for num in rest)


def parse(line: str) -> Program:
    match = re.match(r"(\w+) \((\d+)\)( -> ([\w\s,]+))?", line)
    if match is None:
        raise ValueError(f"Invalid line: {line}")
    group4 = match.group(4)
    return (
        match.group(1),
        int(match.group(2)),
        group4.strip().split(", ") if group4 else [],
    )


def get(programs: list[Program], name: str) -> Program:
    filtered = [program for program in programs if program[0] == name]
    if filtered:
        return filtered[0]
    raise ValueError(f"Invalid name: {name}")


def weight(programs: list[Program], root: Program) -> int:
    _, w, cnames = root
    children = [program for program in programs if program[0] in cnames]
    return w + sum(weight(programs, child) for child in children)


def weights(programs: list[Program], root: Program):
    if not root:
        return []
    _, _, cnames = root
    if not cnames:
        return []
    children = [get(programs, name) for name in cnames]
    return [weight(programs, child) for child in children]


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
