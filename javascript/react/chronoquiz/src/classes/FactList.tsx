import Card from "./Card";
import { default as Color } from "./RgbColorMix";

export default class FactList {
  #items: Array<Card>;

  constructor(cards: Array<Card> = []) {
    this.#items = cards;
  }

  get cards() {
    return this.#items;
  }

  json() {
    return this.#items.map(i => i.json());
  }

  // PRIVATE METHODS
  
  // Set the colors of the cards in this list, in chronological order, to
  // evenly spaced intervals along the spectrum.
  #setColors(): FactList {
    this.sortByDate();
    let items = this.#items;

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

    let items = this.#items;

    for (let i = items.length - 1; i > 0; --i) {
      let j = randomInt(i);
      [items[i], items[j]] = [items[j], items[i]];
    }

    return this;
  }

  // PUBLIC METHODS

  clone(): FactList {
    return new FactList([...this.#items]);
  }

  // Sort the array by the date field, ascending.
  sortByDate(): FactList {
    this.#items.sort((c1, c2) => { return c1.fact.date - c2.fact.date });
    return this;
  }

  sortedByDate(): FactList {
    return this.clone().sortByDate();
  }
  

  setupClues() {
    this.#setColors();
    this.#shuffle();
  }

  get length(): number {
    return this.#items.length;
  }

  isEmpty(): boolean {
    return this.length === 0;
  }

  allButLastItems(): Array<Card> {
    return this.#items.slice(0, -1);
  }

  last(): Card {
    return this.#items.at(-1);
  }

  pop(): Card {
    let card = this.#items.pop();
    return card;
  }

  dropLast(): FactList {
    this.pop();
    return this;
  }

  dropLastCopy(): FactList {
    return new FactList(this.#items.slice(0, -1));
  }

  prepend(item): FactList {
    this.#items.unshift(item);
    return this;
  }
  
  prependCopy(item): FactList {
    return new FactList([item, ...this.#items]);
  }

  // Add event to array and then resort by date.
  addFact(card) {
    this.#items.push(card);
    this.sortByDate();
    return this;
  }

  resetMargins(): FactList {
    let resetItems = [];
    for (let i of this.#items) {
      let card = new Card({...i, expand: false});
      resetItems.push(card);
    }
    return new FactList(resetItems);
  }

  addAnswer(answer): FactList {
    return this.prependCopy(answer).sortedByDate().resetMargins();
  }

  findById(id: string): Card {
    return this.#items.find(c => c.id === id);
  }

  findIndexById(id: string): Card {
    return this.#items.findIndex(c => c.id === id);
  }

  at(index: number): Card {
    return this.#items.at(index);
  }

  map(fn: (Card) => Card): FactList {
    return this.#items.map(fn);
  }

  appendClone(newCard: Card): FactList {
    return this.clone().addFact(newCard);
  }

}

