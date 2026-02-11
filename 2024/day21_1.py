# pylint: disable=missing-module-docstring, missing-function-docstring

import itertools
from typing import Any


def solution(content: list[str]) -> Any:
    assert_efficiency()
    return sum(get_complexity(line, get_sequence(line)) for line in content)


def assert_efficiency():
    for start, end in numpad_to_arrows:
        test1 = transform_arrows(transform_arrows(transform_numpad(end, init=start)))
        test2 = transform_arrows(
            transform_arrows(invert_arrows(transform_numpad(end, init=start)))
        )
        exceptions = {
            ("7", "0"),
            ("7", "A"),
            ("4", "0"),
            ("4", "A"),
            ("1", "0"),
            ("1", "A"),
            ("0", "7"),
            ("0", "4"),
            ("0", "1"),
            ("A", "7"),
            ("A", "4"),
            ("A", "1"),
        }
        if (start, end) not in exceptions:
            assert len(test2) >= len(test1)


def invert_arrows(sequence: str) -> str:
    grouped = [list(g) for _, g in itertools.groupby(sequence[:-1])]
    return "".join("".join(g) for g in grouped[::-1]) + "A"


def get_sequence(line: str) -> str:
    return transform_arrows(transform_arrows(transform_numpad(line.strip())))


def get_complexity(line: str, sequence: str) -> int:
    numeric = int(line.strip()[:-1])
    length = len(sequence)
    return numeric * length


def transform_numpad(code: str, init="A") -> str:
    output = ""
    for start, end in zip([init, *code], code):
        output += numpad_to_arrows[(start, end)] + "A"
    return output


def transform_arrows(code: str, init="A") -> str:
    output = ""
    for start, end in zip([init, *code], code):
        output += arrows_to_arrows[(start, end)] + "A"
    return output


numpad_to_arrows = {
    ("7", "7"): "",
    ("7", "8"): ">",
    ("7", "9"): ">>",
    ("7", "4"): "v",
    ("7", "5"): "v>",
    ("7", "6"): "v>>",
    ("7", "1"): "vv",
    ("7", "2"): "vv>",
    ("7", "3"): "vv>>",
    ("7", "0"): ">vvv",
    ("7", "A"): ">>vvv",
    ("8", "7"): "<",
    ("8", "8"): "",
    ("8", "9"): ">",
    ("8", "4"): "<v",
    ("8", "5"): "v",
    ("8", "6"): "v>",
    ("8", "1"): "<vv",
    ("8", "2"): "vv",
    ("8", "3"): "vv>",
    ("8", "0"): "vvv",
    ("8", "A"): "vvv>",
    ("9", "7"): "<<",
    ("9", "8"): "<",
    ("9", "9"): "",
    ("9", "4"): "<<v",
    ("9", "5"): "<v",
    ("9", "6"): "v",
    ("9", "1"): "<<vv",
    ("9", "2"): "<vv",
    ("9", "3"): "vv",
    ("9", "0"): "<vvv",
    ("9", "A"): "vvv",
    ("4", "7"): "^",
    ("4", "8"): "^>",
    ("4", "9"): "^>>",
    ("4", "4"): "",
    ("4", "5"): ">",
    ("4", "6"): ">>",
    ("4", "1"): "v",
    ("4", "2"): "v>",
    ("4", "3"): "v>>",
    ("4", "0"): ">vv",
    ("4", "A"): ">>vv",
    ("5", "7"): "<^",
    ("5", "8"): "^",
    ("5", "9"): "^>",
    ("5", "4"): "<",
    ("5", "5"): "",
    ("5", "6"): ">",
    ("5", "1"): "<v",
    ("5", "2"): "v",
    ("5", "3"): "v>",
    ("5", "0"): "vv",
    ("5", "A"): "vv>",
    ("6", "7"): "<<^",
    ("6", "8"): "<^",
    ("6", "9"): "^",
    ("6", "4"): "<<",
    ("6", "5"): "<",
    ("6", "6"): "",
    ("6", "1"): "<<v",
    ("6", "2"): "<v",
    ("6", "3"): "v",
    ("6", "0"): "<vv",
    ("6", "A"): "vv",
    ("1", "7"): "^^",
    ("1", "8"): "^^>",
    ("1", "9"): "^^>>",
    ("1", "4"): "^",
    ("1", "5"): "^>",
    ("1", "6"): "^>>",
    ("1", "1"): "",
    ("1", "2"): ">",
    ("1", "3"): ">>",
    ("1", "0"): ">v",
    ("1", "A"): ">>v",
    ("2", "7"): "<^^",
    ("2", "8"): "^^",
    ("2", "9"): "^^>",
    ("2", "4"): "<^",
    ("2", "5"): "^",
    ("2", "6"): "^>",
    ("2", "1"): "<",
    ("2", "2"): "",
    ("2", "3"): ">",
    ("2", "0"): "v",
    ("2", "A"): "v>",
    ("3", "7"): "<<^^",
    ("3", "8"): "<^^",
    ("3", "9"): "^^",
    ("3", "4"): "<<^",
    ("3", "5"): "<^",
    ("3", "6"): "^",
    ("3", "1"): "<<",
    ("3", "2"): "<",
    ("3", "3"): "",
    ("3", "0"): "<v",
    ("3", "A"): "v",
    ("0", "7"): "^^^<",
    ("0", "8"): "^^^",
    ("0", "9"): "^^^>",
    ("0", "4"): "^^<",
    ("0", "5"): "^^",
    ("0", "6"): "^^>",
    ("0", "1"): "^<",
    ("0", "2"): "^",
    ("0", "3"): "^>",
    ("0", "0"): "",
    ("0", "A"): ">",
    ("A", "7"): "^^^<<",
    ("A", "8"): "<^^^",
    ("A", "9"): "^^^",
    ("A", "4"): "^^<<",
    ("A", "5"): "<^^",
    ("A", "6"): "^^",
    ("A", "1"): "^<<",
    ("A", "2"): "<^",
    ("A", "3"): "^",
    ("A", "0"): "<",
    ("A", "A"): "",
}


arrows_to_arrows = {
    ("^", "^"): "",
    ("^", "A"): ">",
    ("^", "<"): "v<",
    ("^", "v"): "v",
    ("^", ">"): "v>",
    ("A", "^"): "<",
    ("A", "A"): "",
    ("A", "<"): "v<<",
    ("A", "v"): "v<",
    ("A", ">"): "v",
    ("<", "^"): ">^",
    ("<", "A"): ">>^",
    ("<", "<"): "",
    ("<", "v"): ">",
    ("<", ">"): ">>",
    ("v", "^"): "^",
    ("v", "A"): ">^",
    ("v", "<"): "<",
    ("v", "v"): "",
    ("v", ">"): ">",
    (">", "^"): "^<",
    (">", "A"): "^",
    (">", "<"): "<<",
    (">", "v"): "<",
    (">", ">"): "",
}


def main() -> None:
    with open("day21.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
