# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Frequencies = list[dict[str, int]]


def solution(content: list[str]) -> Any:
    frequencies: Frequencies = [{} for _ in range(len(content[0].strip()))]
    for line in content:
        for index, char in enumerate(line.strip()):
            frequencies[index].update({char: frequencies[index].get(char, 0) + 1})
    return "".join(
        sorted(f.items(), key=lambda item: item[1])[0][0] for f in frequencies
    )


def main() -> None:
    with open("day06.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
