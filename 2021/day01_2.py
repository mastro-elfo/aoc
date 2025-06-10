# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    depths = [int(x) for x in content]
    windows = [fst + snd + trd for fst, snd, trd in zip(depths, depths[1:], depths[2:])]
    return len([True for (prev, next) in zip(windows, windows[1:]) if next > prev])


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
