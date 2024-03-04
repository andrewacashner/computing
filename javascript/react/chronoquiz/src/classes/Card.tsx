export default class Card {
  isClue: boolean;
  date: Date;
  info: string;
  img: string;
  color: string;
  #safe: boolean;

  // Each card gets the given info and a random unique identifier.
  constructor({
    isClue:boolean = true, // Is this a clue (true) or answer?
    date: number,          // Four-digit year of event
    info: string,          // Brief description of event
    img: string,           // Full URL of image
    color: string          // CSS color to be used in timeline
  }) {
    this.isClue = isClue;
    this.id = crypto.randomUUID();
    this.date = date;
    this.info = info;
    this.img = img;
    this.color = color;
    this.#safe = false; // Has this card been sanitized?
  }


}
