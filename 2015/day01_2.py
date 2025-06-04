# pylint: disable=missing-module-docstring, missing-function-docstring


def solution(content: str) -> None:
    floor = 0
    for index, step in enumerate((content), start=1):
        floor += 1 if step == "(" else -1
        if floor == -1:
            print(index)
            break


def main() -> None:
    with open("day01.dat", "r", encoding="utf8") as file:
        solution(file.read())


if __name__ == "__main__":
    main()
