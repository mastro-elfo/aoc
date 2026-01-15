# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal

type Direction = Literal["L"] | Literal["R"]
type Instruction = tuple[Direction, int]
type State = tuple[int, int]


def solution(content: list[str]) -> Any:
    state = (50, 0)
    for instruction in [parse(line) for line in content]:
        state = rotate(state, instruction)
    return state[1]


def rotate(state: State, instruction: Instruction) -> State:
    position, counter = state
    direction, ticks = instruction

    if ticks == 0:
        return state

    if direction == "L" and position == 0:
        return rotate((99, counter), ("L", ticks - 1))
    if direction == "L" and position - ticks > 0:
        return (position - ticks, counter)
    if direction == "L":
        return rotate((0, counter + 1), ("L", ticks - position))

    if direction == "R" and position == 99:
        return rotate((0, counter + 1), ("R", ticks - 1))
    if direction == "R" and position + ticks < 99:
        return (position + ticks, counter)
    if direction == "R":
        return rotate((99, counter), ("R", ticks - 99 + position))

    raise ValueError(f"Invalid direction: {direction}")


def norm(n: int) -> int:
    if n > 99:
        return n % 100
    while n < 0:
        n += 100
    return n


def parse(line: str) -> Instruction:
    if line.startswith("L"):
        return ("L", int(line[1:]))
    if line.startswith("R"):
        return ("R", int(line[1:]))
    raise ValueError(f"Invalid line: {line}")


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
