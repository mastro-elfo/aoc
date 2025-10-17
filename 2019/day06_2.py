# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Callable, Iterable

type Tree = tuple[str, list[Tree]]


def solution(content: list[str]) -> Any:
    edges = [parse(line) for line in content]
    lefts = [left for left, _ in edges]
    rights = [right for _, right in edges]
    root = [item for item in lefts if item not in rights][0]
    a_tree = tree(root, edges)
    you_branch = branch("YOU", a_tree)
    san_branch = branch("SAN", a_tree)
    you_root, san_root = [
        (yr, sr) for yr, sr in zip(you_branch, san_branch) if yr != sr
    ][0]
    return (
        len(list(drop_while(lambda x: x != you_root, you_branch)))
        + len(list(drop_while(lambda x: x != san_root, san_branch)))
        - 2
    )


def drop_while[T](f: Callable[[T], bool], lst: Iterable[T]):
    found = False
    for item in lst:
        if not found:
            found = not f(item)
        if found:
            yield item


def branch(node: str, tr: Tree) -> list[str]:
    root, rest = tr
    if node == root:
        return [root]
    subtrees = [branch(node, r) for r in rest]
    if any(subtrees):
        return [root] + [s for s in subtrees if s][0]
    return []


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
