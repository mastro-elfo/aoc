# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return sum(get_fuel(int(line)) for line in content)


def get_fuel(mass: int):
    fuel = mass // 3 - 2
    if fuel <= 0:
        return 0
    return fuel + get_fuel(fuel)


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
