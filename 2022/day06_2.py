# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: str) -> Any:
    return 14 + next(
        index
        for index, values in enumerate(
            zip(
                content,
                content[1:],
                content[2:],
                content[3:],
                content[4:],
                content[5:],
                content[6:],
                content[7:],
                content[8:],
                content[9:],
                content[10:],
                content[11:],
                content[12:],
                content[13:],
            )
        )
        if not repeating(values)
    )


def repeating(values: tuple[str, ...]) -> bool:
    a, *rest = values
    if not rest:
        return False
    return a in rest or repeating(tuple(rest))


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
