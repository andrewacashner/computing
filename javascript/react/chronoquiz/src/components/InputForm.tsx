import { useContext, useState, useEffect } from "react";

import Card from "../classes/Card";
import FactList from "../classes/FactList";
import Game from "../classes/Game";

import RestartButton from "./RestartButton";

import TimelineContext from "../store/TimelineContext";

function isInputValid(json: any): boolean {
  return typeof json === "object"
  && Array.isArray(json) 
  && json.length > 0 
  && json.every(fact => ("date" in fact) && ("info" in fact));
}

async function clueListFromJson(json: object): FactList { 
  let cards = [];
  try {
    if (json && json.length > 0) {
      for (let entry of json) {
        try {
          let card = await Card.newSafeCard(entry);
          if (card && card.isSafe) {
            cards.push(card);
          } else {
            throw new Error(`Faulty card input {date: '${entry.date}', info: '${entry.info.slice(0, 15)}...'}; skipping`);
          }
        } catch(e) {
          console.error(e);
        }
      }
    }
  } catch(e) {
    console.error(e);
  }
  return new FactList(cards);
}


export default function InputForm({ src }) {

  let context = useContext(TimelineContext);
  let setGame = context.set;

  let [url, setUrl] = useState("");
  let [json, setJson] = useState(null);
  let [uploadVisible, setUploadVisible] = useState(false);
  let [isGameActive, setIsGameActive] = useState(false);

  function getUrl(event: React.FormEvent<HTMLFormElement>): void {
    event.preventDefault();
    setIsGameActive(true); 
    setUrl(src);
    console.debug(`Set URL to ${src}`);

    //    let source = event.target.source.value;
    //    let files = event.target.fileInput.files;
    //    let newUrl = (files.length > 0) 
    //                  ? URL.createObjectURL(files[0]) 
    //                  : `./input/${source}.json`;
    //    setUrl(newUrl);
  }
  
  useEffect(() => {
    async function fetchUrl(): void {
      if (url) {
        console.log(`Loading file ${url}`);

        try {
          let response = await fetch(url);

          let newJson;
          let contentType = response.headers.get("content-type");
          
          if (contentType && contentType.includes("application/json")) {
            newJson = await response.json();
          } else {
            throw new Error(`No JSON input: ${response}`);
          }

          if (isInputValid(newJson)) {
            setJson(newJson);
          } else {
            alert(`Could not create a timeline from JSON file at ${url}`);
            window.location.reload();
            throw new Error("Unusable JSON input; restarting");
          }
        } catch(e) { 
          console.error(e); 
        }
      }
    }
    fetchUrl();
  }, [url]);

  useEffect(() => {
    async function loadClues(json) {
      try {
        let newClues = await clueListFromJson(json);
        newClues.setupClues();
        setGame(prevGame => new Game({ 
          clues: newClues,
          timeline: prevGame.timeline,
          score: prevGame.score,
          isActive: isGameActive
        }));
      } catch(e) {
        console.error(e);
      }
    }

    loadClues(json);
  }, [json, setGame, isGameActive])


  function showUploadButton(event: React.FormEvent<HTMLFormElement>): void {
    let value = event.target.value;
    setUploadVisible(value === "upload");
  }

  function visibility(isVisible: boolean): string {
    return (isVisible) ? "show" : "hide";
  }


  function SelectTimeline() {
    return(
      <div className={visibility(!isGameActive)}>
        <label htmlFor="source">Choose a timeline:</label>
        <select name="source" id="source" 
          onChange={showUploadButton}
          required defaultValue="music">
          <option value="music">Music</option>
          <option value="wars">Wars</option>
          <option value="upload">Upload a custom timeline...</option>
        </select>
      </div>
    );
  }

  function FileInput() {
    return(
      <div id="file" className={visibility(!isGameActive && uploadVisible)}>
        <label htmlFor="fileInput">Upload JSON timeline file (<a href="about.html">?</a>)</label>
        <input id="fileInput" type="file" accept=".json" />
      </div>
    );
  }

  // TODO "Play again" doesn't work
  function PlayButton() {

    function playMsg(isGameActive: boolean): string {
      return (isGameActive) ? "Play again!" : "Play!";
    }

    return(
      <button type="submit" id="playbutton">{playMsg(isGameActive)}</button>
    );
  }

  // <SelectTimeline />
  // <FileInput />
  return(
    <>
      <form id="inputForm" onSubmit={getUrl}>
        <PlayButton />
      </form>
      {isGameActive ? <RestartButton /> : null }
    </>
  );
}

