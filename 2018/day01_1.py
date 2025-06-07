# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Sequence


def solution(content: Sequence[str]) -> int:
    return sum(map(int, content))


def main() -> None:
    assert solution("+1, -2, +3, +1".split(", ")) == 3
    assert solution("+1, +1, +1".split(", ")) == 3
    assert solution("+1, +1, -2".split(", ")) == 0
    assert solution("-1, -2, -3".split(", ")) == -6

    with open("day01.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
