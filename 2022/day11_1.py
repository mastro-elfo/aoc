# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any, Literal

type WorryLevel = int
type Operation = Literal["+", "*", "2"]
type OperationValue = int
type DivisibilityValue = int
type MonkeyId = int
type MonkeyBusiness = int
type Monkey = tuple[
    MonkeyId,
    list[WorryLevel],
    Operation,
    OperationValue,
    DivisibilityValue,
    MonkeyId,
    MonkeyId,
    MonkeyBusiness,
]
type Monkeys = list[Monkey]


def solution(content: str) -> Any:
    monkeys = parse(content)

    for _ in range(20):
        for _ in monkeys:
            monkeys = monkey_round(monkeys)

    first, second, *_ = sort_by_monkey_business(monkeys)
    return get_business(first) * get_business(second)


def get_business(monkey: Monkey) -> MonkeyBusiness:
    _, _, _, _, _, _, _, monkey_business = monkey
    return monkey_business


def get_id(monkey: Monkey) -> MonkeyId:
    monkey_id, _, _, _, _, _, _, _ = monkey
    return monkey_id


def get_new_level(
    current: int, operation: Operation, operation_value: OperationValue
) -> int:
    level = 0
    if operation == "2":
        level = current**2
    if operation == "*":
        level = current * operation_value
    if operation == "+":
        level = current + operation_value
    return level // 3


def give_item(monkeys: Monkeys, monkey_id: MonkeyId, level: int) -> Monkeys:
    copy: Monkeys = []
    for monkey in monkeys:
        if get_id(monkey) == monkey_id:
            monkey_id, levels, op, opv, dv, tr, fa, mb = monkey
            copy.append((monkey_id, [*levels, level], op, opv, dv, tr, fa, mb))
        else:
            copy.append(monkey)
    return copy


def sort_by_monkey_business(monkeys: Monkeys) -> list[Monkey]:
    return sorted(monkeys, key=get_business, reverse=True)


def parse(content: str) -> Monkeys:
    monkeys: Monkeys = []
    for block in content.split("\n\n"):
        monkeys.append(parse_monkey(block))
    return monkeys


def monkey_round(monkeys: Monkeys) -> Monkeys:
    first, *rest = monkeys
    rest = rest.copy()

    (
        monkey_id,
        levels,
        operation,
        operation_value,
        divisibility,
        truty,
        falsy,
        business,
    ) = first
    for level in levels:
        new_level = get_new_level(level, operation, operation_value)
        rest = give_item(
            rest, truty if (new_level % divisibility == 0) else falsy, new_level
        )

    return [
        *rest,
        (
            monkey_id,
            [],
            operation,
            operation_value,
            divisibility,
            truty,
            falsy,
            business + len(levels),
        ),
    ]


def parse_monkey(block: str) -> Monkey:
    first, second, third, fourth, fifth, sixth, *_ = block.split("\n")
    monkey_id: MonkeyId = int(first.split(":")[0].split(" ")[1])
    starting = [int(x) for x in second.split(":")[1].split(", ")]
    operation_value = third.split(" ")[-1]
    operation: Operation = (
        "2" if operation_value == "old" else ("+" if "+" in third else "*")
    )
    operation_value = 0 if operation == "2" else int(operation_value)
    divisibility_value = int(fourth.split(" ")[-1])
    truty = int(fifth.split(" ")[-1])
    falsy = int(sixth.split(" ")[-1])
    return (
        monkey_id,
        starting,
        operation,
        operation_value,
        divisibility_value,
        truty,
        falsy,
        0,
    )


def main() -> None:
    with open("day11.dat", "r", encoding="utf8") as file:
        print(solution(file.read().strip()))


if __name__ == "__main__":
    main()
