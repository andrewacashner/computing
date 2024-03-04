import Card from "Card";

export default class FactList extends Array {
  constructor(...cards: Array<Card>) {
    super(...cards);
  }
}

