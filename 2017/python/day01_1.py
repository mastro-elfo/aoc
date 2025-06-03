# pylint: disable=missing-module-docstring, missing-function-docstring


def solution(content: str) -> None:
    print(sum(int(d) for d, e in zip(content, content[1:] + content[0]) if d == e))


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        solution(file.read().strip())


if __name__ == "__main__":
    main()
