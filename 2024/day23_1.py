# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Edge = tuple[str, str]


def solution(content: list[str]) -> Any:
    edges = [parse(line) for line in content]
    groups_of_3 = set()
    for index, (fst, snd) in enumerate(edges):
        thirds = (
            {other for other, f in edges[index:] if f == fst}
            | {other for f, other in edges[index:] if f == fst}
        ) & (
            {other for other, f in edges[index:] if f == snd}
            | {other for f, other in edges[index:] if f == snd}
        )
        groups_of_3 = groups_of_3.union(
            {
                (tuple(sorted((fst, snd, trd))))
                for trd in thirds
                if any(x.startswith("t") for x in (fst, snd, trd))
            }
        )

    return len(groups_of_3)


def parse(line: str) -> Edge:
    fst, snd, *_ = sorted(line.strip().split("-"))
    return fst, snd


def main() -> None:
    with open("day23.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
