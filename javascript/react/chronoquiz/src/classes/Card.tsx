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
    this.date = date;
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

  /** Create a new card with sanitized input.
   *
   * - The date must be an integer string <= the current year (including
   *   negative numbers).
   * - The info is converted to plain text using textContent.
   * - The image, if present, is downloaded and cached.
   *
   * The parameters are the same as for new Card().
   *
   * Returns {Card} - Card with validated content (with safe property set to
   * true), or null if the input was invalid
   */
  static async newSafeCard({ isClue, date, info, img, color }: {
    isClue?: boolean, 
    date?: number, 
    info?: string, 
    img?: string, 
    color?: color
  }): Card {
    try {
      let cleanDate = Card.#sanitizeDate(date);
      let cleanInfo = Card.#sanitizeInfo(info);
      let cleanImg = await Card.#sanitizeImg(img).catch(e => console.error(e));

      // The date is the only dealbreaker. We just skip a bad image link.
      if (cleanDate) {
        let card = new Card({
          isClue: isClue, 
          date: cleanDate, 
          info: cleanInfo, 
          img: cleanImg, 
          color: color
        });
        card.#markSafe();
        return card;
      } else {
        throw new Error(`Could not sanitize card input with date '${date}', info '${info}'`);
      }
    } catch(e) {
      console.error(e);
    }
  }

  // PRIVATE METHODS
  // Sanitize input 
  static #sanitizeDate(raw: string | number | Date): Date | null {
    try {
      let numTest = Number(raw);
      if (!isNaN(numTest) 
        && Number.isInteger(numTest) 
        && numTest <= new Date().getFullYear()) {

        let date = new Date();
        date.setFullYear(numTest);
        return date;
      } else {
        throw new Error(`Bad date input ${raw}`);
      }
    } catch(e) { 
      console.error(e);
    }
  }

  static #sanitizeInfo(raw: string): string {
    let node = document.createElement("span");
    node.textContent = raw;
    return node.textContent;
  }

  static async #sanitizeImg(url: string): string | null {
    function doesImageExist(url: string): boolean { 
      return new Promise((resolve, reject) => {
        const img = new Image();
        img.src = url;
        img.onload = () => resolve(true);
        img.onerror = () => resolve(false);
      });
    }
    if (!url) {
      return null;
    } else {
      let imgTest = await doesImageExist(url).catch(e => console.log(err));
      if (imgTest === true) {
        return url;
      } else {
        throw new Error(`Image not found at url '${url}'`);
      }
    } 
  }


}
