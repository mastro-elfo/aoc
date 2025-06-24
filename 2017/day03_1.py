# pylint: disable=missing-module-docstring, missing-function-docstring


import math
from typing import Any


def solution(content: int) -> Any:
    if content < 1:
        raise ValueError(f"Invalid input: {content}")
    if content == 1:
        return 0
    if content < 10:
        return 1 if content % 2 == 0 else 2
    side = math.ceil(content**0.5)
    side = side if side % 2 == 1 else side + 1
    half = side // 2
    diff = min(
        abs(x - content)
        for x in range(side * side - half, (side - 2) * (side - 2), 1 - side)
    )
    return half + diff


def main() -> None:
    print(solution(325489))


if __name__ == "__main__":
    main()
