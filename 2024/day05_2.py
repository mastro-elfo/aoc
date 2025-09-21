# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Rule = tuple[int, int]


def solution(content: str) -> Any:
    [fst, snd] = content.split("\n\n")
    rules = parse_rules(fst)
    pages = parse_pages(snd)
    return sum(
        middle(correct(page_group, rules))
        for page_group in pages
        if any(is_invalid(rule, page_group) for rule in rules)
    )


def correct(pages: list[int], rules: list[Rule]) -> list[int]:
    for rule in rules:
        if is_invalid(rule, pages):
            [fst, snd] = rule
            corrected = pages.copy()
            fst_index = corrected.index(fst)
            snd_index = corrected.index(snd)
            corrected[fst_index], corrected[snd_index] = (
                corrected[snd_index],
                corrected[fst_index],
            )
            return correct(corrected, rules)
    return pages


def middle(pages: list[int]) -> int:
    if not pages:
        return 0
    return pages[len(pages) // 2]


def is_invalid(rule: Rule, pages: list[int]) -> bool:
    [fst, snd] = rule
    if fst not in pages or snd not in pages:
        return False
    return pages.index(fst) > pages.index(snd)


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
