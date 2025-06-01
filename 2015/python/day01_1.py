# pylint: disable=missing-module-docstring, missing-function-docstring


def solution(content: str) -> None:
    print(
        len(list(True for step in content if step == "("))
        - len(list(True for step in content if step == ")"))
    )


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        solution(file.read())


if __name__ == "__main__":
    main()
