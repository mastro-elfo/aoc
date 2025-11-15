# pylint: disable=missing-module-docstring, missing-function-docstring


from typing import Any

type Step = tuple[str, list[str]]
type Worker = tuple[str, int]

WORKERS = 5
TIME = 60


def solution(content: list[str]) -> Any:
    names = set(line.split(" ")[1] for line in content) | set(
        line.split(" ")[-3] for line in content
    )
    steps: list[Step] = [(name, parse_requirements(content, name)) for name in names]
    return unwind(steps)


def unwind(steps: list[Step]) -> int:
    copy = steps.copy()
    count = 0
    workers: list[Worker] = [("", 0)] * WORKERS
    output = ""
    while copy:
        workers = [(step, max(0, time - 1)) for step, time in workers]

        for worker in workers:
            if not is_idle(worker) and has_ended(worker):
                name, _ = worker
                output += name
                copy = [step for step in copy if step[0] != name]
                copy = [
                    (nm, [req for req in reqs if req != name]) for (nm, reqs) in copy
                ]
        workers = [worker if not has_ended(worker) else ("", 0) for worker in workers]

        working_steps = [name for name, _ in workers]
        available_steps = sorted(
            step[0] for step in copy if not step[1] and step[0] not in working_steps
        )

        available_workers = [
            index for index, worker in enumerate(workers) if is_idle(worker)
        ]
        for step, index in zip(available_steps, available_workers):
            workers[index] = (step, TIME + job_time(step))

        count += 1

    return count - 1


def job_time(name: str) -> int:
    return ord(name) - ord("A") + 1


def has_ended(worker: Worker):
    _, time = worker
    return time == 0


def is_idle(worker: Worker):
    step, _ = worker
    return not step


def parse_requirements(content: list[str], name: str) -> list[str]:
    return [line.split(" ")[1] for line in content if line.split(" ")[-3] == name]


def main() -> None:
    with open("day07.dat", "r", encoding="utf8") as file:
        print(solution(file.readlines()))


if __name__ == "__main__":
    main()
