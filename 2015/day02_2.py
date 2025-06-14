# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Dimensions = tuple[int, int, int]


def solution(content: list[str]) -> Any:
    return sum(ribbon(parse(line)) for line in content)


def ribbon(dims: Dimensions):
    a, b = sorted(dims)[:2]
    return 2 * a + 2 * b + dims[0] * dims[1] * dims[2]


def parse(line: str):
    dims = [int(x) for x in line.split("x")]
    return (dims[0], dims[1], dims[2])


def main() -> None:
    with open("day02.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
