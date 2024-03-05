import { useState, useEffect } from "react";
import Card from "../classes/Card";
import FactList from "../classes/FactList";

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

async function loadTimeline(event: React.FormEvent<HTMLFormElement> = null): FactList {

  let files = event.currentTarget.fileInput.files as FileList;
  let source = event.currentTarget.source.value as string;
  let url = getInputUrl(source, files);

  if (url) {
    console.log(`Loading file ${url}`);
    let cards = [];

    let response = await fetch(url).catch(e => {
      console.error(e);
      return cards;
    });

    let data = await response.json().catch(e => {
      console.error(e);
      return cards;
    });

    cards = await cardArrayFromJson(data).catch(e => {
      console.error(e);
      throw new Error("Could not create cards from JSON input");
    });

    return new FactList(cards);
  }
}

// TODO doesn't work
// TODO return type
function useTimelineSelection(event: React.FormEvent<HTMLFormElement>) {
  let setClues = useState(new FactList())[1];
  let cards = useEffect(() => loadTimeline(event), [event]);
  setClues(prev => cards);
  return loadTimeline;
}

export default useTimelineSelection;
