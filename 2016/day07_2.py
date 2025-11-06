# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type IPV7 = tuple[list[str], list[str]]


def solution(content: list[str]) -> Any:
    return len(
        [ip for ip in ((parse(line.strip())) for line in content) if support_ssl(ip)]
    )


def support_ssl(ip: IPV7) -> bool:
    net, hnet = ip
    abas = [aba for part in net for aba in get_abas(part)]
    return any(get_bab(aba) in part for part in hnet for aba in abas)


def get_bab(aba: str):
    return "".join((aba[1], aba[0], aba[1]))


def get_abas(part: str) -> list[str]:
    return [
        "".join((a, b, c))
        for a, b, c in zip(part, part[1:], part[2:])
        if a == c and a != b
    ]


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
