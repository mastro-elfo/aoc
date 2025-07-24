# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from datetime import datetime, timedelta
from typing import Any

type Guard = tuple[int, str, str]


def solution(content: list[str]) -> Any:
    guards = group_guard_and_minute(parse(sorted(content)))
    _, key, val = sorted(
        [(v, m, key) for key, guard in guards.items() for m, v in guard.items()],
        reverse=True,
    )[0]
    return key * val


def group_guard_and_minute(timeshift: list[Guard]):
    guards: dict[int, dict[int, int]] = {}
    for shift in timeshift:
        guard_id, start, end = shift
        guards[guard_id] = guards.get(guard_id, {})
        current = datetime.fromisoformat(start)
        end_dt = datetime.fromisoformat(end)
        while current < end_dt:
            guards[guard_id][current.minute] = (
                guards[guard_id].get(current.minute, 0) + 1
            )
            current = current + timedelta(minutes=1)
    return guards


def parse(lines: list[str]):
    guard_id = None
    output = []
    start = None
    for line in lines:
        matches = re.match(r"\[(\d{4}-\d{2}-\d{2} \d{2}:\d{2})\] (.+)", line)
        if matches is None:
            raise ValueError(f"Invalid line {line}")
        begins = re.match(r"Guard #(\d+) begins shift", matches.group(2))
        asleep = "falls asleep" == matches.group(2)
        wakesup = "wakes up" == matches.group(2)
        if begins is not None:
            guard_id = begins.group(1)
        elif asleep:
            start = matches.group(1)
        elif wakesup and guard_id is not None:
            output.append((int(guard_id), start, matches.group(1)))
    return output


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
