# pylint: disable=missing-module-docstring, missing-function-docstring

from multiprocessing import Pool
from typing import Any

type Table = tuple[int, int, int]


def solution(content: list[str]) -> Any:
    maps = parse(content[2:])
    seeds = parse_seeds(content[0])
    with Pool(10) as p:
        return min(
            p.map(
                process,
                [(seeds, maps) for seeds in seeds],
            )
        )


def process(data: tuple[range, list[list[Table]]]):
    seeds, maps = data
    minimum = None
    for seed in seeds:
        loc = convert(seed, maps)
        minimum = loc if minimum is None else min(minimum, loc)
    if minimum is None:
        raise ValueError("Invalid minimum")
    return minimum


def parse_seeds(line: str):
    values = [int(x) for x in line[7:].split(" ")]
    return [
        (range(start, start + size)) for start, size in zip(values[::2], values[1::2])
    ]


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
