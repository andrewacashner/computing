import { useContext } from "react";
import TimelineContext from "../store/TimelineContext";

export default function Clues() {
  let context = useContext(TimelineContext);
  console.log(context.get);
  let clues = context.get.clues;
  console.log("Found clues");
  console.log(clues);
  let cards = clues.map(clue => <p key={clue.id}>{clue.fact}</p>);

  return(
    <div className="clue">{cards}</div>
  );
}
