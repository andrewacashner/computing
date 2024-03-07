import Card from "Card";
import { default as Color } from "./RgbColorMix";

export default class FactList {
  items: Array<Card>;

  constructor(cards: Array<Card> = []) {
    this.items = cards;
  }

  // PRIVATE METHODS
  
  // Sort the array by the date field, ascending.
  #sortByDate(): FactList {
    this.items.sort((c1, c2) => { return c1.date - c2.date });
    return this;
  }
  
  // Set the colors of the cards in this list, in chronological order, to
  // evenly spaced intervals along the spectrum.
  #setColors(): FactList {
    this.#sortByDate();
    let items = this.items;

    items.forEach((card, index) => {
      items[index].color = Color.colorAtIndex(index, items.length, 
                                              Color.SPECTRUM);
    });
    return this;
  } 

  // Shuffle the array, using the Fisher-Yates/Knuth shuffle
  // (`https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle`)
  #shuffle(): FactList {
    
    function randomInt(max: number): number {
      return Math.floor(Math.random() * max);
    } 

    let items = this.items;

    for (let i = items.length - 1; i > 0; --i) {
      let j = randomInt(i);
      [items[i], items[j]] = [items[j], items[i]];
    }

    return this;
  }

  // PUBLIC METHODS
  setupClues() {
    this.#setColors();
    this.#shuffle();
  }

  isEmpty(): boolean {
    return this.items.length === 0;
  }

  allButLast(): Array<Card> {
    return this.items.slice(0, -1);
  }

  last(): Card {
    return this.items.at(-1);
  }

  pop(): Card {
    let card = this.items.pop();
    return card;
  }

  // Add event to array and then resort by date.
  addFact(card) {
    this.items.push(card);
    this.#sortByDate();
  }

}

