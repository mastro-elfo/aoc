# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Step = tuple[str, list[str]]


def solution(content: list[str]) -> Any:
    names = set(line.split(" ")[1] for line in content) | set(
        line.split(" ")[-3] for line in content
    )
    steps: list[Step] = [(name, parse_requirements(content, name)) for name in names]
    return unwind(steps)


def unwind(steps: list[Step]) -> str:
    output = ""
    copy = steps.copy()
    while copy:
        first, *_ = sorted(step[0] for step in copy if not step[1])
        output += first
        copy = [step for step in copy if step[0] != first]
        copy = [(name, [req for req in reqs if req != first]) for (name, reqs) in copy]

    return output


def parse_requirements(content: list[str], name: str) -> list[str]:
    return [line.split(" ")[1] for line in content if line.split(" ")[-3] == name]


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
