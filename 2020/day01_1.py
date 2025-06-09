# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return find(sorted(int(x) for x in content if x))


def find(expenses: list[int]):
    fst, *rest, lst = expenses
    result = fst + lst
    if result > 2020:
        return find([fst] + rest)
    if result < 2020:
        return find(rest + [lst])
    return fst * lst


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
