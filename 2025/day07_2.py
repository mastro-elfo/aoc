# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return propagate(
        find_start(content[0]),
        list(
            enumerate(
                [line for line in content[1:] if any(ch != "." for ch in line)],
            )
        ),
    )


cache: dict[tuple[int, int], int] = {}


def propagate(beam: int, content: list[tuple[int, str]]) -> int:
    if not content:
        return 1
    (row, line), *rest = content
    key = (row, beam)
    cached = cache.get(key)
    if cached is not None:
        return cached
    if line[beam] == "^":
        value = propagate(beam - 1, rest) + propagate(beam + 1, rest)
        cache[key] = value
        return value
    return propagate(beam, rest)


def find_start(line: str) -> int:
    return line.index("S")


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
