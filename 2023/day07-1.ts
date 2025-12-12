type Poker =
  | "HighCard"
  | "OnePair"
  | "TwoPair"
  | "ThreeOfKind"
  | "FullHouse"
  | "FourOfKind"
  | "FiveOfKind";

type Hand = { cards: string; type: Poker; bid: number };

function solution(content: string) {
  return content
    .split("\n")
    .map(parse)
    .toSorted(compareCards)
    .toSorted(compareType)
    .map((hand, index) => hand.bid * (index + 1))
    .reduce((acc, cur) => acc + cur, 0);
}

function compareType(a: Hand, b: Hand) {
  return (
    [
      "HighCard",
      "OnePair",
      "TwoPair",
      "ThreeOfKind",
      "FullHouse",
      "FourOfKind",
      "FiveOfKind",
    ].indexOf(a.type) -
    [
      "HighCard",
      "OnePair",
      "TwoPair",
      "ThreeOfKind",
      "FullHouse",
      "FourOfKind",
      "FiveOfKind",
    ].indexOf(b.type)
  );
}

function compareCards(a: Hand, b: Hand): number {
  return (
    a.cards
      .split("")
      .map(
        (_, index) =>
          "23456789TJQKA".indexOf(a.cards.at(index)!) -
          "23456789TJQKA".indexOf(b.cards.at(index)!)
      )
      .find((diff) => diff !== 0) ?? 0
  );
}

function parseType(cards: string): Poker {
  const signature = Object.values(
    cards.split("").reduce((acc, cur) => {
      if (acc[cur]) return { ...acc, [cur]: acc[cur] + 1 };
      return { ...acc, [cur]: 1 };
    }, {} as { [k: string]: number })
  )
    .toSorted()
    .join(",");

  if (signature === "5") return "FiveOfKind";
  if (signature === "1,4") return "FourOfKind";
  if (signature === "2,3") return "FullHouse";
  if (signature === "1,1,3") return "ThreeOfKind";
  if (signature === "1,2,2") return "TwoPair";
  if (signature === "1,1,1,2") return "OnePair";
  return "HighCard";
}

function parse(line: string): Hand {
  const [cards, bid] = line.split(" ");
  return {
    cards,
    type: parseType(cards),
    bid: Number(bid),
  };
}

Deno.readTextFile("day07.dat")
  .then((content) => content.trim())
  .then(solution)
  .then(console.log);
