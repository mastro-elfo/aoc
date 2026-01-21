# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]
type Antenna = tuple[str, Coords]


def solution(content: list[str]) -> Any:
    width = len(content[0].strip())
    height = len(content)
    antennas = parse(content)
    labels = set(label for label, _ in antennas)
    antinodes: set[Coords] = set()
    by_label = [
        [coords for lab, coords in antennas if lab == label] for label in labels
    ]
    for group in by_label:
        for current in group:
            for other in group:
                if current == other:
                    continue
                an = vdif(vscale(current, 2), other)
                if is_inside(width, height, an):
                    antinodes.add(an)
    return len(antinodes)


def is_inside(width: int, height: int, vec: Coords) -> bool:
    vx, vy = vec
    return vx >= 0 and vy >= 0 and vx < width and vy < height


def vscale(vec: Coords, k: int) -> Coords:
    vx, vy = vec
    return (vx * k, vy * k)


def vdif(v1: Coords, v2: Coords) -> Coords:
    v1x, v1y = v1
    v2x, v2y = v2
    return (v1x - v2x, v1y - v2y)


def parse(content: list[str]) -> list[Antenna]:
    antennas: list[Antenna] = []
    for row_index, row in enumerate(content):
        for col_index, char in enumerate(row.strip()):
            if char != ".":
                antennas.append((char, (row_index, col_index)))
    return antennas


def main() -> None:
    with open("day08.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
