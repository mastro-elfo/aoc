# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any

type Tile = int
type Region = tuple[int, list[int]]


def solution(content: str) -> Any:
    tiles, regions = parse(content)
    return len([True for region in regions if test(tiles, region)])


def test(tiles: list[int], region: Region) -> bool:
    (area, reqs) = region

    lower = sum(r * s for r, s in zip(reqs, tiles))
    if area < lower:
        return False

    upper = sum(r * 9 for r in reqs)
    if upper <= area:
        return True

    raise ValueError(f"Unknown result for: {region}")


def parse(content: str) -> tuple[list[Tile], list[Region]]:
    parts = content.split("\n\n")
    return (
        [parse_tile(block) for block in parts[:-1]],
        [parse_region(line) for line in parts[-1].split("\n")],
    )


def parse_tile(block: str) -> Tile:
    return sum(char == "#" for char in block)


def parse_region(line: str) -> Region:
    match_region = re.match(r"^(\d+)x(\d+): (.+)$", line)
    if not match_region:
        raise ValueError(f"Invalid region: {line}")

    return (
        int(match_region[1]) * int(match_region[2]),
        [int(x) for x in match_region[3].split(" ")],
    )


def main() -> None:
    with open("day12.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
