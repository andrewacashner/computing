import { useContext } from "react";
import TimelineContext from "../store/TimelineContext";
import Card from "./Card";

export default function Clues() {
  let context = useContext(TimelineContext);
  let game = context.get;
  let timeline = game.timeline;

  if (game.isActive && timeline && !timeline.isEmpty()) {
    return(
      <div className="timeline">
        {timeline.items.map(card => <Card key={card.id}>{card}</Card>)}
      </div>
    );
  }
}
