# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return len([line for line in content if is_nice(line)])


def is_nice(line: str):
    return all(f(line) for f in [has_efe_rule, has_repeating_double])


def has_repeating_double(line: str):
    return any(
        x + y in line[index + 2 :] for index, (x, y) in enumerate(zip(line, line[1:]))
    )


def has_efe_rule(line: str):
    return any(x == y for x, y in zip(line, line[2:]))


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
