# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any, Literal

type Lens = tuple[str, int]
type Box = list[Lens]
type Operation = Literal["=", "-"]
type Instruction = tuple[str, Operation, int]


def solution(content: str) -> Any:
    boxes: list[Box] = [[] for _ in range(256)]
    for instruction in (parse(instruction) for instruction in content.split(",")):
        boxes = initialize(boxes, instruction)
    return focusing_power(boxes)


def focusing_power(boxes: list[Box]) -> int:
    return sum(
        box_no * lens_no * focal
        for box_no, lenses in enumerate(boxes, start=1)
        for lens_no, (_, focal) in enumerate(lenses, start=1)
    )


def initialize(boxes: list[Box], instruction: Instruction) -> list[Box]:
    label, op, focal = instruction
    box_index = hashfn(label)
    copy = boxes.copy()
    if op == "-":
        copy[box_index] = [lens for lens in copy[box_index] if get_label(lens) != label]
    elif op == "=":
        if has_lens(copy[box_index], label):
            copy[box_index] = replace(copy[box_index], label, focal)
        else:
            copy[box_index].append((label, focal))
    return copy


def replace(box: Box, label: str, focal: int) -> Box:
    copy = box.copy()
    for index, (lbl, _) in enumerate(copy):
        if lbl == label:
            copy[index] = (label, focal)
    return copy


def has_lens(box: Box, label: str) -> bool:
    return label in (get_label(lens) for lens in box)


def get_label(lens: Lens) -> str:
    label, _ = lens
    return label


def get_focal(lens: Lens) -> int:
    _, focal = lens
    return focal


def parse(instruction: str) -> Instruction:
    matches = re.match(r"(\w+)([-=])(\d)?", instruction)
    if matches is None:
        raise ValueError(f"Invalid instruction: {instruction}")
    return (
        matches[1],
        "=" if matches[2] == "=" else "-",
        int(matches[3]) if matches[3] else 0,
    )


def hashfn(rts: str) -> int:
    current = 0
    for char in rts:
        current = (17 * (current + ord(char))) % 256
    return current


def main() -> None:
    with open("day15.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
