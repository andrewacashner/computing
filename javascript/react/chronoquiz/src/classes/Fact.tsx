export default class Fact {
  id: number;
  date: Date;
  info: string;
  img: string;

  constructor({ id = -1, date = new Date(), info = "", img = "" } = {}) {
    this.id = id;
    this.date = date;
    this.info = info;
    this.img = img;
  }

  static newFromYear({ id, date, info, img }) {
    let realDate = new Date();
    realDate.setFullYear(date);
    let event = new Fact({ id: id, date: realDate, info: info, img: img });
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
      id: this.id,
      date: this.year,
      info: this.info,
      img: this.img
    }
  }
}
