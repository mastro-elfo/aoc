# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Rule = tuple[int, int]


def solution(content: str) -> Any:
    [fst, snd] = content.split("\n\n")
    rules = parse_rules(fst)
    pages = parse_pages(snd)
    return sum(
        (
            middle(page_group)
            for page_group in pages
            if all(is_valid(rule, page_group) for rule in rules)
        )
    )


def middle(pages: list[int]) -> int:
    return pages[len(pages) // 2]


def is_valid(rule: Rule, pages: list[int]) -> bool:
    [fst, snd] = rule
    if fst not in pages or snd not in pages:
        return True
    return pages.index(fst) < pages.index(snd)


def parse_rules(content: str) -> list[Rule]:
    return [
        (int(fst), int(snd))
        for [fst, snd] in [line.split("|") for line in content.split("\n")]
    ]


def parse_pages(content: str) -> list[list[int]]:
    return [[int(page) for page in line.split(",")] for line in content.split("\n")]


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
