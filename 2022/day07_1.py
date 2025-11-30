# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal

type FileType = Literal["F"] | Literal["D"]
type FileName = list[str]
type FileSize = int

type File = tuple[FileName, FileSize, FileType]

type State = tuple[FileName, list[File]]


def solution(content: list[str]) -> Any:
    files = parse(content)
    return sum(
        value
        for value in (size(files, file) for file in files if is_directory(file))
        if value < 100_000
    )


def is_directory(file: File) -> bool:
    _, _, tp = file
    return tp == "D"


def file_name(file: File) -> FileName:
    name, _, _ = file
    return name


def contains(f1: File, f2: File) -> bool:
    return "".join(file_name(f2)).endswith("".join(file_name(f1)))


def file_size(file: File) -> FileSize:
    _, fsize, _ = file
    return fsize


def size(files: list[File], file: File) -> FileSize:
    return sum(file_size(item) for item in files if contains(file, item))


def parse(content: list[str]) -> list[File]:
    state = ([], [])
    for line in content:
        state = parse_line(state, line)
    _, files = state
    return files


def parse_line(state: State, line: str) -> State:
    clean = line.strip()
    if clean.startswith("$ ls"):
        return state
    current, files = state
    if clean.startswith("$ cd .."):
        return (current[1:], files)
    if clean.startswith("$ cd "):
        return ([clean[5:], *current], files)
    if clean.startswith("dir "):
        return (current, [([clean[4:], *current], 0, "D"), *files])
    fsize, fname, *_ = clean.split(" ")
    return (current, [([fname, *current], int(fsize), "F"), *files])


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
