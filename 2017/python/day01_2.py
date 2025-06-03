# pylint: disable=missing-module-docstring, missing-function-docstring


def solution(content: str) -> None:
    ahead = len(content) // 2
    print(
        sum(
            int(d)
            for d, e in zip(content, content[ahead:] + content[0:ahead])
            if d == e
        )
    )


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        solution(file.read().strip())


if __name__ == "__main__":
    main()
