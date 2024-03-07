import Card from "./Card";
import FactList from "./FactList";

export default class Game {
  clues: FactList;
  timeline: FactList;
  score: number;

  constructor({ clues, timeline, score = 0 }: {
    clues?: FactList, 
    timeline?: FactList, 
    score?: number
  }) {
    this.clues = clues;
    this.timeline = timeline;
    this.score = score;
  }

  static startingGame() {
    const NowCard = new Card({
      isClue: false, 
      date: new Date().getFullYear(),
      info: "Now", 
      img: "https://images.pexels.com/photos/17139860/pexels-photo-17139860/free-photo-of-hourglass-with-sand.jpeg",
      //      color: violet
    });
    // TODO make safe

    return new Game({
      clues: new FactList(), 
      timeline: new FactList([NowCard]), 
      score: 0
    });
  }

  get isActive() {
    return this.clues && !this.clues.isEmpty();
  }
}
