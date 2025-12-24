# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type MemoryCell = tuple[int, int]
type Memory = dict[MemoryCell, int]


def solution(content: int) -> Any:
    memory: Memory = {(0, 0): 1}
    value = 1
    g = generator()
    current = next(g)
    while value < content:
        current = next(g)
        value = sum(memory[c] for c in neighbors(current, memory))
        memory[current] = value
    return value


def neighbors(current: MemoryCell, memory: Memory):
    cx, cy = current
    return [
        (mx, my)
        for mx, my in memory
        if abs(mx - cx) <= 1 and abs(my - cy) <= 1 and (mx, my) != (cx, cy)
    ]


def generator():
    side = 1
    current = (0, 0)
    yield current

    while True:
        side += 2
        current = (current[0] + 1, current[1])
        yield current
        while current[1] < side // 2:
            current = (current[0], current[1] + 1)
            yield current
        while current[0] > -side // 2 + 1:
            current = (current[0] - 1, current[1])
            yield current
        while current[1] > -side // 2 + 1:
            current = (current[0], current[1] - 1)
            yield current
        while current[0] < side // 2:
            current = (current[0] + 1, current[1])
            yield current


# TODO: refactor by reading from file
def main() -> None:
    print(solution(325489))


if __name__ == "__main__":
    main()
