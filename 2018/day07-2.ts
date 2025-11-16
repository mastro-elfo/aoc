type Step = { name: string; requires: string[] };
type Worker = { step: string; time: number };

function solution(content: string) {
  const lines = content.split("\n");
  const names = new Set(
    lines
      .map((line) => line.split(" ")[1])
      .concat(lines.map((line) => line.split(" ").at(-3)!))
  );
  const steps: Step[] = Array.from(names).map((name) => ({
    name,
    requires: lines
      .map((line) => line.split(" "))
      .filter((items) => items.at(-3) === name)
      .map((items) => items[1]),
  }));
  return unwind(steps);
}

function unwind(steps: Step[]) {
  let copy = steps.slice();
  let count = 0;
  let workers: Worker[] = Array.from({ length: 5 }).map(() => ({
    step: "",
    time: 0,
  }));

  while (copy.length) {
    workers = workers.map(({ step, time }) => ({
      step,
      time: Math.max(0, time - 1),
    }));

    workers.forEach((worker) => {
      if (!isIdle(worker) && hasEnded(worker)) {
        const { step } = worker;
        copy = copy.filter(({ name }) => name !== step);
        copy = copy.map(({ name, requires }) => ({
          name,
          requires: requires.filter((req) => req !== step),
        }));
      }
    });
    workers = workers.map((worker) =>
      hasEnded(worker) ? { step: "", time: 0 } : worker
    );

    const workingSteps = workers.map(({ step }) => step);
    const availableSteps = copy
      .filter(
        ({ name, requires }) => !workingSteps.includes(name) && !requires.length
      )
      .map(({ name }) => name)
      .toSorted();

    const availableWorkers = workers
      .map((worker, index) => (isIdle(worker) ? index : -1))
      .filter((index) => index !== -1);

    availableSteps.forEach((step, index) => {
      const workerIndex = availableWorkers[index];
      if (!isNaN(workerIndex)) {
        workers[workerIndex] = { step, time: 60 + jobTime(step) };
      }
    });

    count += 1;
  }
  return count - 1;
}

function jobTime(name: string) {
  return name.charCodeAt(0) - "A".charCodeAt(0) + 1;
}

function hasEnded(worker: Worker) {
  return worker.time === 0;
}

function isIdle(worker: Worker) {
  return !worker.step;
}

Deno.readTextFile("day07.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
