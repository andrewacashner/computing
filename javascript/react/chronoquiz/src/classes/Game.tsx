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
  isActive: boolean;
  isGameOver: boolean;

  constructor({ clues, timeline, score = 0, isActive = false, isGameOver = false }: GameInput = {}) {
    this.clues = clues;
    this.timeline = timeline;
    this.score = score;
    this.isActive = isActive;
    this.isGameOver = isGameOver;
  }
  // PRIVATE METHODS
  
  /** Remove last item of clue list and add it to timeline list.
   * (Note: The timeline list will then shuffle itself in chronological
   * order.)
   */
  moveCurrentClueToTimeline(): Game {
    let answer = this.clues.pop();
    answer.isClue = false;
    this.timeline.addFact(answer);
    return this;
  }

  copyWithNextClue(): Game {
    let answer = this.clues.last().copyAsAnswer();
    let newTimeline = this.timeline.addAnswer(answer);
    
    let newClues = this.clues.dropLastCopy();
    let gameOver = newClues.isEmpty();

    return new Game({
      clues: newClues,
      timeline: newTimeline,
      score: this.score,
      isActive: this.isActive,
      isGameOver: gameOver
    });
  }

  // PUBLIC METHODS
 
  // Start the game with just a "Now" card in the timeline.
  // Since we have the image locally we don't need to sanitize the card.
  static startingGame(): Game {
    let NowCard = new Card({
      isClue: false, 
      date: new Date(),
      info: "Now", 
      img: "../img/hourglass.jpg",
      color: Color.VIOLET
    });
    NowCard.markSafe();

    return new Game({
      clues: new FactList(), 
      timeline: new FactList([NowCard]),
      score: 0
    });
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

}
