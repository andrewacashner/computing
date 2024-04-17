// Menu to choose which timeline to load (or create new)

import { useState } from "react";
import Timeline from "../classes/Timeline";

interface AdminChooserInput {
  timelines: array<Timeline>,
  setID: (id: number) => void,
}

export default function AdminChooser({ 
  timelines, setID 
}: AdminChooserInput): React.ReactElement {

  function loadTimeline(event: React.FormEvent<HTMLFormElement>): void {
    event.preventDefault();
    let data = new FormData(event.target);
    let id = data.get("select-timeline");
    setID(id);
  }

  function timelineOption(timeline: Timeline): React.ReactElement {
    return(
      <option key={timeline.id} value={timeline.id}>{timeline.title}</option>
    );
  }

  // Change display of submit button whether creating or loading
  let [selection, setSelection] = useState("create");

  function updateSelection(event: React.FormEvent<HTMLFormElement>): void {
    setSelection(event.target.value);
  }

  let loadButtonText = (selection === "create") ? "Create" : "Load";

  return(
    <form id="chooser" onSubmit={loadTimeline}>
      <label htmlFor="select-timeline">Select a Timeline:</label>
      <select name="select-timeline" 
        defaultValue="create" 
        onChange={updateSelection}>
        <option value="create">Create New</option>
        { timelines.map(timelineOption) }
      </select>
      <button type="submit">{ loadButtonText }</button>
    </form>
  );
}
