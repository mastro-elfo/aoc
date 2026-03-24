# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Report = tuple[str, list[int]]


def solution(content: list[str]) -> Any:
    return sum(count_arrangments(parse_report(line)) for line in content)


def count_arrangments(report: Report) -> int:
    schema, groups = report
    total = len(schema)
    broken = sum(groups)
    parts = len(groups)
    count = 0
    for arrangment in generate(total - broken, parts + 1):
        if any(x == 0 for x in arrangment[1:-1]):
            continue
        candidate = make_schema(arrangment, groups)
        if test(schema, candidate):
            count += 1
    return count


def test(ref: str, candidate: str) -> bool:
    return all(r in ("?", c) for r, c in zip(ref, candidate))


def make_schema(arrangment: list[int], broken: list[int]) -> str:
    return "".join("." * a + "#" * b for a, b in zip(arrangment, broken + [0]))


def generate(n: int, p: int):
    if n == 0:
        yield [0] * p
    elif p == 0:
        yield [0]
    elif p == 1:
        yield [n]
    else:
        for i in range(n + 1):
            for rest in generate(i, p - 1):
                yield [n - i] + rest


def parse_report(line: str) -> Report:
    schema, _, groups = line.partition(" ")
    return (schema, [int(x) for x in groups.split(",")])


def main() -> None:
    with open("day12.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
