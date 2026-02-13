# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Generator, Iterable, Sequence

type Seller = dict[tuple[int, int, int, int], int]


def solution(content: list[str]) -> Any:
    sellers: list[Seller] = [get_seller(int(line)) for line in content]
    keys = {key for seller in sellers for key in seller}
    return second(
        first(
            sorted(
                ((key, sum(seller.get(key, 0) for seller in sellers)) for key in keys),
                key=second,
                reverse=True,
            )
        )
    )


def get_seller(start: int) -> Seller:
    seller = {}
    for price, key in group_last4_diff(unit(take(2001, generate_next_secret(start)))):
        if key not in seller:
            seller[key] = price
    return seller


def first[T](value: Sequence[T]) -> T:
    fst, *_ = value
    return fst


def second[T](value: Sequence[T]) -> T:
    _, snd, *_ = value
    return snd


def take[T](n: int, sequence: Iterable[T]):
    return (value for _, value in zip(range(n), sequence))


def unit(sequence: Iterable[int]):
    return (int(str(value)[-1]) for value in sequence)


def group_last4_diff(sequence: Generator[int]):
    two = next(sequence)
    three = next(sequence)
    four = next(sequence)
    price = next(sequence)

    while True:
        one = two
        two = three
        three = four
        four = price
        try:
            price = next(sequence)
            yield (price, (price - four, four - three, three - two, two - one))
        except StopIteration:
            break


def generate_next_secret(secret: int):
    yield secret
    while True:
        secret = mix_and_prune(secret, secret * 64)
        secret = mix_and_prune(secret, secret // 32)
        secret = mix_and_prune(secret, secret * 2048)
        yield secret


def mix_and_prune(secret: int, number: int) -> int:
    return (secret ^ number) % 16_777_216


def main() -> None:
    with open("day22.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
