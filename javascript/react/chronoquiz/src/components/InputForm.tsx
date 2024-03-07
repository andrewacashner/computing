import { useState, useContext, useEffect } from "react";
import TimelineContext from "../store/TimelineContext";
import useTimelineSelection from "../hooks/useTimelineSelection";

async function fetchTimeline(url) {
  let response = await fetch(url);
  let json = await response.json();
  return json;
}

export default function InputForm() {

  let context = useContext(TimelineContext);
  let setGame = context.set;

  let [url, setUrl] = useState("./input/music.json");

  function getUrl(event: React.FormEvent<HTMLFormElement>): void {
    let source = event.target.source.value;
    let files = event.target.fileInput.files;
    if (files.length > 0) { 
      setUrl(URL.createObjectURL(files[0]));
    } else {
      setUrl(`./input/${source}.json`);
    }
  }

  let [json, setJson] = useState("{ 'status': 'placeholder' }");
  
  useEffect(() => {
    console.log(`Loading file '${url}'`);
    fetchTimeline("./input/music.json")
    .then(response => setJson(response))
    .catch(console.error);
  }, [url]);

  return(
    <form id="inputForm" onSubmit={getUrl}>
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

