# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any


def solution(content: list[str]) -> Any:
    return sum(get_next_secret(int(line), 2000) for line in content)


def get_next_secret(secret: int, count: int) -> int:
    for _ in range(count):
        secret = mix_and_prune(secret, secret * 64)
        secret = mix_and_prune(secret, secret // 32)
        secret = mix_and_prune(secret, secret * 2048)
    return secret


def mix_and_prune(secret: int, number: int) -> int:
    return (secret ^ number) % 16_777_216


def main() -> None:
    with open("day22.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
