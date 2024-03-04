import React from 'react';
import './App.css';

import { useState } from "react";

import Game from "./classes/Game";

import Instructions from "./components/Instructions";
import InputForm from "./components/InputForm";
import ScorePanel from "./components/ScorePanel";
import Clues from "./components/Clues";
import Timeline from "./components/Timeline";

import TimelineContext from "./store/TimelineContext";

function App() {
  let [game, setGame] = useState(new Game());
  let gameContext = { get: game, set: setGame };

  return (
    <TimelineContext.Provider value={gameContext}>
      <div className="App">
        <header>
          <h1>Chronoquiz</h1>
          <Instructions />
          <InputForm  />
          <ScorePanel />
        </header>
        <main>
          <Clues />
          <Timeline />
        </main>
      </div>
    </TimelineContext.Provider>
  );
}

export default App;
