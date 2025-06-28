# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Coords = tuple[int, int]
type Instruction = tuple[str, int]
type Segment = tuple[Coords, Coords]


def solution(content: str) -> Any:
    lines = content.split("\n")
    return sorted(
        [
            manhattan((0, 0), coords)
            for s1, s2 in [
                (s1, s2)
                for s1 in parse(lines[0])
                for s2 in parse(lines[1])
                if intersect(s1, s2)
            ]
            for coords in unfold(s1)
            if coords in unfold(s2) and coords != (0, 0)
        ]
    )[0]


def unfold(segment: Segment):
    start, end = segment
    if is_vertical(segment) and start[1] < end[1]:
        s, e = (start[1], end[1]) if start[1] < end[1] else (end[1], start[1])
        return [(start[0], y) for y in range(s, e + 1)]
    else:
        s, e = (start[0], end[0]) if start[0] < end[0] else (end[0], start[0])
        return [(x, start[1]) for x in range(s, e + 1)]


def intersect(s1: Segment, s2: Segment):
    start1, end1 = s1
    start2, end2 = s2
    if is_vertical(s1) and is_vertical(s2):
        return start1[1] == start2[1] and (
            between(start1[1], start2[1], end2[1])
            or between(end1[1], start2[1], end2[1])
            or between(start2[1], start1[1], end1[1])
            or between(end2[1], start1[1], end1[1])
        )
    if not is_vertical(s1) and is_vertical(s2):
        return between(start1[1], start2[1], end2[1]) and between(
            start2[0], start1[0], end1[0]
        )
    if is_vertical(s1) and not is_vertical(s2):
        return between(start1[0], start2[0], end2[0]) and between(
            start2[1], start1[1], end1[1]
        )
    if not is_vertical(s1) and not is_vertical(s2):
        return start1[0] == start2[0] and (
            (
                between(start1[0], start2[0], end2[0])
                or between(end1[0], start2[0], end2[0])
                or between(start2[0], start1[0], end1[0])
                or between(end2[0], start1[0], end1[0])
            )
        )
    return False


def is_vertical(seg: Segment):
    start, end = seg
    return start[0] == end[0]


def between(x: int, start: int, end: int):
    return (start <= x <= end) if start < end else (end <= x <= start)


def parse(line: str) -> list[Segment]:
    segments = []
    for instruction in [parse_instruction(word) for word in line.split(",")]:
        if segments:
            _, last = segments[-1]
            segments.append((last, move(last, instruction)))
        else:
            segments.append(((0, 0), move((0, 0), instruction)))
    return segments


def move(start: Coords, instr: Instruction):
    if instr[0] == "R":
        return (start[0] + instr[1], start[1])
    if instr[0] == "L":
        return (start[0] - instr[1], start[1])
    if instr[0] == "U":
        return (start[0], start[1] + instr[1])
    if instr[0] == "D":
        return (start[0], start[1] - instr[1])
    return start


def parse_instruction(word: str) -> Instruction:
    return (word[0], int(word[1:]))


def manhattan(c1: Coords, c2: Coords):
    return abs(c1[0] - c2[0]) + abs(c1[1] - c2[1])


def main() -> None:
    with open("day03.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
