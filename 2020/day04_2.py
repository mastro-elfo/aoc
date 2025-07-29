# pylint: disable=missing-module-docstring, missing-function-docstring


import re
from typing import Any


def solution(content: str) -> Any:
    return len(
        [doc for doc in content.split("\n\n") if is_valid(" ".join(doc.split("\n")))]
    )


def is_valid(line: str):
    return all(
        f(line)
        for f in [
            is_valid_byr,
            is_valid_iyr,
            is_valid_eyr,
            is_valid_hgt,
            is_valid_hcl,
            is_valid_ecl,
            is_valid_pid,
        ]
    )


def is_valid_pid(line: str):
    match = re.match(
        r".*?pid:([0-9]+)",
        line,
    )
    return bool(match and len(match.group(1)) == 9)


def is_valid_ecl(line: str):
    return bool(re.match(r".*?ecl:(amb|blu|brn|gry|grn|hzl|oth)", line))


def is_valid_hcl(line: str):
    return bool(re.match(r".*?hcl:#[0-9a-f]{6}", line))


def is_valid_hgt(line: str):
    match = re.match(r".*?hgt:(\d+)(cm|in)", line)
    if match is None:
        return False
    if match.group(2) == "cm":
        return 150 <= int(match.group(1)) <= 193
    return 59 <= int(match.group(1)) <= 76


def is_valid_eyr(line: str):
    match = re.match(r".*?eyr:(\d\d\d\d)", line)
    return bool(match and 2020 <= int(match.group(1)) <= 2030)


def is_valid_iyr(line: str):
    match = re.match(r".*?iyr:(\d\d\d\d)", line)
    return bool(match and 2010 <= int(match.group(1)) <= 2020)


def is_valid_byr(line: str):
    match = re.match(r".*?byr:(\d\d\d\d)", line)
    return bool(match and 1920 <= int(match.group(1)) <= 2002)


def main() -> None:
    with open("day04.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
