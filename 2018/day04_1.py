# pylint: disable=missing-module-docstring, missing-function-docstring


import datetime
import re
from typing import Any

type Guard = tuple[str, str, str]


def solution(content: list[str]) -> Any:
    guards = parse(sorted(content))
    worst_id = worst(guards)
    worst_guard = [g for g in guards if g[0] == worst_id]
    minute = worst_minute(worst_guard)
    return int(worst_id) * int(minute)


def worst_minute(guards: list[Guard]):
    return sorted(by_minute(guards).items(), key=lambda x: x[1], reverse=True)[0][0]


def by_minute(guards: list[Guard]):
    minutes = {}
    for g in guards:
        _, start, end = g
        current = datetime.datetime.fromisoformat(start)
        end_dt = datetime.datetime.fromisoformat(end)
        while current < end_dt:
            minutes[current.minute] = minutes.get(current.minute, 0) + 1
            current = current + datetime.timedelta(minutes=1)
    return minutes


def worst(guards: list[Guard]):
    return sorted(by_guard(guards).items(), key=lambda x: x[1], reverse=True)[0][0]


def by_guard(timeshift: list[Guard]):
    guards = {}
    for shift in timeshift:
        guard_id, start, end = shift
        guards[guard_id] = guards.get(guard_id, 0) + difference(start, end)
    return guards


def difference(start: str, end: str):
    return (
        datetime.datetime.fromisoformat(end) - datetime.datetime.fromisoformat(start)
    ).seconds // 60


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
        elif wakesup:
            output.append((guard_id, start, matches.group(1)))
    return output


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
