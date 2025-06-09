# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    expenses = sorted(int(x) for x in content if x)
    for index, e in enumerate(expenses):
        result = find(2020 - e, expenses[:index] + expenses[index + 1 :])
        if result is not None:
            return result * e


def find(target: int, expenses: list[int]):
    if len(expenses) < 2:
        return None
    fst, *rest, lst = expenses
    result = fst + lst
    if result > target:
        return find(target, [fst] + rest)
    if result < target:
        return find(target, rest + [lst])
    return fst * lst


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
