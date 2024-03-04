import FactList from "./FactList";

export default class Game {
  clues: FactList;
  timeline: FactList;
  score: number;

  constructor(
    clues: FactList = new FactList(),
    timeline: FactList = new FactList(), 
    score: number = 0) {
    this.clues = clues;
    this.timeline = timeline;
    this.score = score;
  }
}
