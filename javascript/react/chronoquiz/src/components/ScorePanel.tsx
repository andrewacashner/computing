import { useContext } from "react";
import TimelineContext from "../store/TimelineContext";

export default function ScorePanel() {
  let context = useContext(TimelineContext);
  let score = context.get.score;
  return(
    <>
      <p>Score: <span className="score">{score}</span></p>
      <button id="restart">Play Again</button>
    </>
  );
}
