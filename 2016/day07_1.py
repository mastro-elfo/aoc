# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type IPV7 = tuple[list[str], list[str]]


def solution(content: list[str]) -> Any:
    return len(
        [ip for ip in ((parse(line.strip())) for line in content) if support_tls(ip)]
    )


def support_tls(ip: IPV7) -> bool:
    net, hnet = ip
    return any(has_abba(part) for part in net) and not any(
        has_abba(part) for part in hnet
    )


def has_abba(part: str) -> bool:
    if len(part) < 4:
        return False
    return any(
        a == d and b == c and a != b
        for a, b, c, d in zip(part, part[1:], part[2:], part[3:])
    )


def parse(line: str) -> IPV7:
    nets: list[str] = []
    hnets: list[str] = []
    parse_hnet = False
    if line[0] != "[":
        nets.append("")
    for ch in line:
        if ch == "[":
            parse_hnet = True
            hnets.append("")
        elif ch == "]":
            parse_hnet = False
            nets.append("")
        elif parse_hnet:
            hnets[-1] += ch
        else:
            nets[-1] += ch
    return (nets, hnets)


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
