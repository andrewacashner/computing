import Card from "./Card";
import FactList from "./FactList";
import { default as Color } from "./RgbColorMix";

interface GameInput {
  clues: FactList;
  timeline: FactList;
  score: number;
}

export default class Game {
  clues: FactList;
  timeline: FactList;
  score: number;

  constructor({ clues, timeline, score = 0 }: GameInput) {
    this.clues = clues;
    this.timeline = timeline;
    this.score = score;
  }
  // PRIVATE METHODS
  
  /** Remove last item of clue list and add it to timeline list.
   * (Note: The timeline list will then shuffle itself in chronological
   * order.)
   */
  #moveCurrentClueToTimeline() {
    let answer = this.clues.pop();
    answer.isClue = false;
    this.timeline.addFact(answer);
  }

  // PUBLIC METHODS
 
  // Start the game with just a "Now" card in the timeline.
  // Since we have the image locally we don't need to sanitize the card.
  static startingGame(): Game {
    let NowCard = new Card({
      isClue: false, 
      date: new Date(),
      info: "Now", 
      img: "./img/hourglass.jpg",
      color: Color.VIOLET
    });
    NowCard.markSafe();

    return new Game({
      clues: new FactList(), 
      timeline: new FactList([NowCard]),
      score: 0
    });
  }

  get isActive(): boolean {
    return this.clues && !this.clues.isEmpty();
  }

  incrementScore(): Game {
    ++this.score;
    return this;
  }

  // Subtract one from score; don't go below zero
  decrementScore(): Game {
    this.score  = Math.max(0, this.score - 1);
    return this;
  }

  // Check for end of game, otherwise pull up the next clue.
  nextClue(): Game {
    this.#moveCurrentClueToTimeline();
    if (!this.clues.isEmpty()) {
      updateClues(this);
    } else {
      // TODO
      this.isActive = false;
      //      displayGameOver(this.score);
    }
    return this;
  }

}
