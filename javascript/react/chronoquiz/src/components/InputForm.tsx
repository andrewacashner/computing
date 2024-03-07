import { useContext, useState, useEffect } from "react";

import Card from "../classes/Card";
import FactList from "../classes/FactList";
import Game from "../classes/Game";

import TimelineContext from "../store/TimelineContext";

export default function InputForm() {

  let context = useContext(TimelineContext);
  let setGame = context.set;

  let [url, setUrl] = useState("");
  let [json, setJson] = useState(null);
  let [uploadVisible, setUploadVisible] = useState(false);

  function getUrl(event: React.FormEvent<HTMLFormElement>): void {
    event.preventDefault();
    let source = event.target.source.value;
    let files = event.target.fileInput.files;
    let newUrl = (files.length > 0) 
                  ? URL.createObjectURL(files[0]) 
                  : `./input/${source}.json`;
    setUrl(newUrl);
  }

  useEffect(() => {
    async function fetchUrl(): void {
      if (url) {
        console.log(`Loading file ${url}`);

        try {
          let response = await fetch(url);

          let contentType = response.headers.get("content-type");
          let json;
          if (contentType && contentType.includes("application/json")) {
            json = await response.json();
          } else {
            throw new Error(`No JSON input: ${response}`);
          }

          if (json) {
            setJson(json);
          } else {
            throw new Error(`Unusable JSON input: ${newJson}`);
          }
        } catch(e) { 
          console.error(e); 
        }
      }
    }
    fetchUrl();
  }, [url]);

  useEffect(() => {
    if (json && json.length > 0) {
      let cards = [];
      for (let entry of json) {
        let card = new Card({isClue: true, ...entry});
        cards.push(card);
      }
      let newClues = new FactList(cards);
      setGame(prev => new Game({
        clues: newClues,
        timeline: prev.timeline,
        score: prev.score
      }));
    }
  }, [json, setGame])


  function showUploadButton(event: React.FormEvent<HTMLFormElement>): void {
    let value = event.target.value;
    setUploadVisible(value === "upload");
  }

  function visibility(isVisible: boolean): string {
    return (isVisible) ? "show" : "hide";
  }

  return(
    <form id="inputForm" onSubmit={getUrl}>
      <label htmlFor="source">Choose a timeline:</label>
      <select name="source" id="source" 
        onChange={showUploadButton}
        required defaultValue="music">
        <option value="music">Music</option>
        <option value="wars">Wars</option>
        <option value="upload">Upload a custom timeline...</option>
      </select>
      <div id="file" className={visibility(uploadVisible)}>
        <label htmlFor="fileInput">Upload JSON timeline file (<a href="about.html">?</a>)</label>
        <input id="fileInput" type="file" accept=".json" />
      </div>
      <button type="submit" id="playbutton">Play!</button>
    </form>
  );
}

