import { useContext, useEffect } from "react";
import TimelineContext from "../store/TimelineContext";
import useTimelineSelection from "../hooks/useTimelineSelection";

export default function InputForm() {

  let context = useContext(TimelineContext);
  let setGame = context.set;

  // TODO doesn't work (infinite loop of re-renders)
  let loadTimeline = useTimelineSelection(null, setGame);
  useEffect(() => loadTimeline());

  function setupTimeline(event: React.FormEvent<HTMLFormElement>): void {
    loadTimeline(event, setGame);
  }

  return(
    <form id="inputForm" onSubmit={setupTimeline}>
      <label htmlFor="source">Choose a timeline:</label>
      <select name="source" id="source" required defaultValue="music">
        <option value="music">Music</option>
        <option value="wars">Wars</option>
        <option value="upload">Upload a custom timeline...</option>
      </select>
      <div id="file" className="hide">
        <label htmlFor="fileInput">Upload JSON timeline file (<a href="about.html">?</a>)</label>
        <input id="fileInput" type="file" accept=".json" />
      </div>
      <button type="submit" id="playbutton">Play!</button>
    </form>
  );
}

