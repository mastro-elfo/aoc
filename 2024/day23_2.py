# pylint: disable=missing-module-docstring, missing-function-docstring


import itertools
from typing import Any, Sequence

type Edge = tuple[str, str]
type Hash = tuple[str, ...]


def solution(content: list[str]) -> Any:
    edges = {parse(line) for line in content}
    nodes = {fst for fst, _ in edges} | {snd for _, snd in edges}
    ashes = sorted(hashfn(edges, node) for node in nodes)

    lst1 = sorted(
        tuple(sorted(set(row) & set(col)))
        for row in ashes
        for col in ashes
        # TODO: it works because the largest group has 13 nodes
        # Should consider the lowest distance
        if distance(row, col) < 2
    )

    lst2 = (
        (len(list(group)), intersection)
        for intersection, group in itertools.groupby(lst1)
    )
    return ",".join(first(sorted(lst2, key=first, reverse=True))[1])


def first[T](sequence: Sequence[T]) -> T:
    fst, *_ = sequence
    return fst


def second[T](sequence: Sequence[T]) -> T:
    _, snd, *_ = sequence
    return snd


def third[T](sequence: Sequence[T]) -> T:
    _, _, trd, *_ = sequence
    return trd


def distance(ash1: Hash, ash2: Hash) -> int:
    if ash1 == ash2:
        return 0
    return sum(0 if a in ash2 else 1 for a in ash1)


def hashfn(edges: set[Edge], node: str) -> tuple[str, ...]:
    edges_with_node = {edge for edge in edges if node in edge}
    ash = {fst for fst, _ in edges_with_node} | {snd for _, snd in edges_with_node}
    return tuple(sorted(ash))


def parse(line: str) -> Edge:
    fst, snd, *_ = sorted(line.strip().split("-"))
    return fst, snd


def main() -> None:
    with open("day23.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
