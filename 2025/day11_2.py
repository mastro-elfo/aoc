# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Devices = dict[str, frozenset[str]]


def solution(content: list[str]) -> Any:
    return count(
        {key: value for key, value in (parse(line) for line in content)},
        "svr",
        False,
        False,
    )


cache: dict[tuple[str, bool, bool], int] = {}


def count(deivces: Devices, name: str, fft: bool, dac: bool) -> int:
    if name == "out":
        return 1 if fft and dac else 0
    key = (name, fft, dac)
    if key in cache:
        return cache[key]
    result = sum(
        count(deivces, target, fft or (name == "fft"), dac or (name == "dac"))
        for target in deivces[name]
    )
    cache[key] = result
    return result


def parse(line: str) -> tuple[str, frozenset[str]]:
    first, *rest = line.split(" ")
    return (first[:-1], frozenset(item.strip() for item in rest))


def main() -> None:
    with open("day11.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
