type MemoryCell = { x: number; y: number };
type Memory = Record<string, number>;

function solution(target: number) {
  const memory: Memory = { "0|0": 1 };
  let value = 1;
  const g = generator();
  let current = g.next();
  while (value < target) {
    current = g.next();
    value = neighbors(current.value!, memory)
      .map((item) => memory[hash(item)])
      .reduce((acc, cur) => acc + cur, 0);
    memory[hash(current.value!)] = value;
  }
  return value;
}

function neighbors(current: MemoryCell, memory: Memory) {
  const { x: cx, y: cy } = current;
  return Object.keys(memory)
    .map(parse)
    .filter(
      ({ x, y }) =>
        Math.abs(x - cx) <= 1 && Math.abs(y - cy) <= 1 && (cx !== x || cy !== y)
    );
}

function* generator() {
  let side = 1;
  let current = { x: 0, y: 0 };
  yield current;
  while (true) {
    side += 2;
    current = { x: current.x + 1, y: current.y };
    yield current;
    while (current.y < Math.floor(side / 2)) {
      current = { x: current.x, y: current.y + 1 };
      yield current;
    }
    while (current.x > Math.floor(-side / 2) + 1) {
      current = { x: current.x - 1, y: current.y };
      yield current;
    }
    while (current.y > Math.floor(-side / 2) + 1) {
      current = { x: current.x, y: current.y - 1 };
      yield current;
    }
    while (current.x < Math.floor(side / 2)) {
      current = { x: current.x + 1, y: current.y };
      yield current;
    }
  }
}

function parse(key: string): MemoryCell {
  const match = key.match(/(-?\d+)\|(-?\d+)/);
  if (!match) throw new Error(`Invalid key: ${key}`);
  return { x: Number(match[1]), y: Number(match[2]) };
}

function hash({ x, y }: MemoryCell): string {
  return `${x}|${y}`;
}

console.log(solution(325489));
