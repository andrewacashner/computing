export default class Fact {
  date: Date;
  info: string;
  img: string;

  constructor({ date = new Date(), info = "", img = "" } = {}) {
    this.date = date;
    this.info = info;
    this.img = img;
  }

  static fromYear({ year, info, img }) {
    let date = new Date().setFullYear(year);
    let event = new Fact({ date: date, info: info, img: img });
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
    return (this.year < 0) ? `${-this.year} BCE` : `${this.year}`;
  }

  json() {
    return {
      date: this.year,
      info: this.info,
      img: this.img
    }
  }
}
