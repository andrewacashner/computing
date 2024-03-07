export default class Card {
  isClue: boolean;
  id: string;
  date: Date;
  info: string;
  img: string;
  color: string;
  #safe: boolean;

  // Each card gets the given info and a random unique identifier.
  constructor({ isClue = true, date, info, img, color }: {
    isClue?: boolean, // Is this a clue (true) or answer?
    date?: number,    // Four-digit year of event
    info?: string,    // Brief description of event
    img?: string,     // Full URL of image
    color?: string    // CSS color to be used in timeline
  }) {
    this.isClue = isClue;
    this.id = crypto.randomUUID();
    this.date = new Date();
    this.date.setFullYear(date);
    this.info = info;
    this.img = img;
    this.color = color;
    this.#safe = false; // Has this card been sanitized?
  }

  #markSafe(): Card {
    this.#safe = true;
    return this;
  }

  get isSafe(): boolean {
    return this.#safe;
  }

  /**
   * Return the year if positive or year BC if negative. (Deals with the year
   * only.) 
   *
   * Technically BC should be offset by one year but we told users to use
   * negative numbers as years BC.
   *
   * Returns: Formatted string for year, with BC if the year was
   * negative
   */
  dateToString(): string { 
    if (this.isClue) {
      return "Clue";
    } else {
      let yearZero = new Date();
      yearZero.setFullYear(0);

      let displayYear = this.year;
      if (this.date < yearZero) {
        displayYear = `${-displayYear} bce`; 
      } 
      return displayYear;
    }
  }
  
  // PUBLIC METHODS

  // Return the date as YYYY year string.
  get year(): string { return this.date.getFullYear(); }
 
  set year(YYYY: string): void { 
    if (YYYY) {
      this.date.setFullYear(YYYY); 
    } 
  }

}
