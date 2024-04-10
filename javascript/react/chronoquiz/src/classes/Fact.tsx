export default class Fact {
  date: Date;
  info: string;
  img: string;

  constructor({ date = new Date(), info = "", img = "" } = {}) {
    this.date = date;
    this.info = info;
    this.img = img;
  }

  static newFromYear({ date, info, img }) {
    let realDate = new Date();
    realDate.setFullYear(date);
    let event = new Fact({ date: realDate, info: info, img: img });
    return event;
  }

  get year(): number {
    return this.date.getFullYear();
  }

  set year(numYear: number) {
    if (year === 0) {
      throw new Error("Year cannot equal zero. Use 1 or -1.");
    } else {
      this.date.setFullYear(numYear);
    }
  }

  get yearString(): string {
    // bce is lowercase because the card date uses small-caps font 
    return (this.year < 0) ? `${-this.year} bce` : `${this.year}`;
  }

  json() {
    return {
      date: this.year,
      info: this.info,
      img: this.img
    }
  }
}
