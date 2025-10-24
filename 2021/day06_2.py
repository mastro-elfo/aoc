# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type FishGroup = tuple[int, int]


def solution(content: str) -> Any:
    groups = parse(content)
    for _ in range(256):
        groups = play(groups)
    return sum(size for _, size in groups)


def play(groups: list[FishGroup]) -> list[FishGroup]:
    return clean(next_day(generate(groups)))


def clean(groups: list[FishGroup]) -> list[FishGroup]:
    copy = groups.copy()
    copy = [(day, size) for day, size in copy if day != 6]
    copy.append((6, sum(size for day, size in groups if day == 6)))
    copy = [(day, size) for day, size in copy if size != 0]
    return copy


def next_day(groups: list[FishGroup]) -> list[FishGroup]:
    return [(6 if day == 0 else (day - 1), size) for day, size in groups]


def generate(groups: list[FishGroup]) -> list[FishGroup]:
    copy = groups.copy()
    copy.extend((9, size) for day, size in groups if day == 0)
    return copy


def parse(content: str) -> list[FishGroup]:
    data = [int(x) for x in content.split(",")]
    return [(day, len([d for d in data if d == day])) for day in set(data)]


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
