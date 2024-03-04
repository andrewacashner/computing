import { useState, useContext, useEffect } from "react";
import TimelineContext from "../store/TimelineContext";
import FactList from "../classes/FactList";
import Game from "../classes/Game";

export default function InputForm() {

  let context = useContext(TimelineContext);
  let game = context.get;
  let setGame = context.set;

  function userUploadUrl(files: FileList): string {
    return URL.createObjectURL(files[0]);
  }

  function getInputUrl(source: string, files: FileList): string {
    let url = "";
    if (source) {
      if (files.length > 0) {
        url = userUploadUrl(files);
      } else {
        url = `input/${source}.json`;
      }
    }
    return url;
  }

  async function cardArrayFromJson(json: Array<{date, info}>): Array<Card> {
    let cards = [];
    for (let entry of json) {
      try {
        let validCard = await Card.newSafeCard(entry);
        if (validCard && validCard.isSafe) {
          cards.push(validCard);
        } else {
          throw new Error(`Faulty card input {date: ${entry.date}, info: ${entry.info}}; skipping`);
        } 
      } catch(e) {
        console.error(e);
      }
    }
    return cards;
  }

  async function loadTimeline(url: string): FactList | null {
    let response = await fetch(url).catch(e => {
      console.error(e);
      return [];
    });
    let data = await response.json().catch(e => {
      console.error(e);
      return [];
    });

    let cards = await cardArrayFromJson(data).catch(e => {
      console.error(e);
      return null;
    });
    return cards;
  }


  async function setupTimeline(event: React.FormEvent<HTMLFormElement>): Timeline | null{

    let files = event.currentTarget.fileInput.files as FileList;
    let source = event.currentTarget.source.value as string;

    if (url) {
      console.log(`Loading file ${url}`);
      let clues = await loadTimeline(url);
      if (clues) {
        setGame(prevGame => new Game(clues, prevGame.timeline, prevGame.score));
      }
    } else {
      throw new Error("Invalid input, cannot play game.");
    }
  }

  // TODO doesn't work
  function useTimeline(event: React.FormEvent<HTMLFormElement>): void {
    useEffect(() => setupTimeline(event), [game]);
  }

  return(
    <form id="inputForm" onSubmit={useTimeline}>
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

