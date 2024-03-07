import Card from "Card";

export default class FactList {
  items: Array<Card>;

  constructor(cards: Array<Card> = []) {
    this.items = cards;
  }

  isEmpty() {
    return this.items.length === 0;
  }

  allButLast() {
    return this.items.slice(0, -1);
  }

  last() {
    return this.items.at(-1);
  }
}

