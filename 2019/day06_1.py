# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Tree = tuple[str, list[Tree]]


def solution(content: list[str]) -> Any:
    edges = [parse(line) for line in content]
    lefts = [left for left, _ in edges]
    rights = [right for _, right in edges]
    root = [item for item in lefts if item not in rights][0]
    return count(tree(root, edges))


def count(tr: Tree):
    def helper(value: int, t: Tree) -> int:
        _, rest = t
        return value + sum(helper(value + 1, r) for r in rest)

    return helper(0, tr)


def tree(root: str, edges: list[tuple[str, str]]) -> Tree:
    return (
        root,
        [tree(b, edges) for a, b in edges if a == root],
    )


def parse(line: str) -> tuple[str, str]:
    parts = line.split(")")
    return (parts[0].strip(), parts[1].strip())


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
