import { useState, useEffect } from "react";

import User from "../classes/User";
import TimelineList from "../components/TimelineList";

export default function Choose() {
  let [timelineList, setTimelineList] = useState([]);

  useEffect(() => {
    async function loadTimelineList() {
      let response = await fetch(`${User.SERVER}/timelines/`);
      if (response.ok) {
        let json = await response.json();
        setTimelineList(json);
      }
    }

    loadTimelineList();
  }, []);
  
  return(
    <main>
      <h1>Choose a Quiz</h1>
      <TimelineList data={timelineList} />
    </main>
  );
}
