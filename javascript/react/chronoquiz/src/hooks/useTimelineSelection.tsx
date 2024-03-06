import { useState, useEffect } from "react";
import Card from "../classes/Card";
import FactList from "../classes/FactList";
import Game from "../classes/Game";

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

function useUrl(
  event: React.FormEvent<HTMLFormElement> = null,
  setGameFn: (Game) => void
): void {

  let [cards, setCards] = useState([]);

  if (event) {
    let files = event.currentTarget.fileInput.files as FileList;
    let source = event.currentTarget.source.value as string;
    let url = getInputUrl(source, files);

    console.log(`Loading file ${url}`);
    useEffect(function () {
      fetch(url).then(
        function (response) {
          let json = response.json();
          let cardArray = cardArrayFromJson(json);
          setCards(prev => cardArray);
        }).catch(e => {
          console.error(e)
          throw new Error("Could not create cards from JSON input");
        })
    }, [cards]);

    if (cards) {
      let cardList = new FactList(cards);
      setGameFn(prev => new Game(cardList));
    }
  }
}

// TODO doesn't work
// TODO return type
function useTimelineSelection(event: React.FormEvent<HTMLFormElement>,
setGameFn: (Game) => void): 
  (event: React.FormEvent<HTMLFormElement>, setGameFn: (Game) => void) => void {

  useEffect(() => {
    async function load() {
      useUrl(event, setGameFn).catch(e => console.log(e));
    }
    load();
  }, [event, setGameFn]);

  return (event, setGameFn) => {
    async function load() {
      useUrl(event, setGameFn).catch(e => console.log(e));
    }
    load();
  }
}

export default useTimelineSelection;
