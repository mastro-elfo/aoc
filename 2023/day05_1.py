# pylint: disable=missing-module-docstring, missing-function-docstring

from typing import Any

type Table = tuple[int, int, int]


def solution(content: list[str]) -> Any:
    maps = parse(content[2:])
    seeds = parse_seeds(content[0])
    return min(convert(seed, maps) for seed in seeds)


def parse_seeds(line: str):
    return [int(x) for x in line[7:].split(" ")]


def parse(lines: list[str]) -> list[list[Table]]:
    output: list[list[Table]] = []
    for line in lines:
        if line.strip() == "":
            continue
        elif "-" in line:
            output.append([])
        else:
            output = output[:-1] + [output[-1] + [parse_one(line)]]
    return output


def parse_one(line: str) -> Table:
    [dst, src, size] = [int(x) for x in line.split(" ")]
    return (dst, src, size)


def convert(seed: int, maps: list[list[Table]]) -> int:
    if not maps:
        return seed
    [fst, *rest] = maps
    for dst, src, size in fst:
        if src <= seed < src + size:
            return convert(dst + seed - src, rest)
    return convert(seed, rest)


def main() -> None:
    with open("day05.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
