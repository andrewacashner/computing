import React from 'react';
import logo from './logo.svg';
import './App.css';

import { useState } from "react";

import FactList from "./classes/FactList";

import Instructions from "./components/Instructions";
import InputForm from "./components/InputForm";
import ScorePanel from "./components/ScorePanel";
import Clues from "./components/Clues";
import Timeline from "./components/Timeline";

function App() {
  // TODO use context instead of props
  let [clues, setClues] = useState<FactList>(new FactList());
  let [score, setScore] = useState<number>(0);

  function play() {
    console.log("PLAY");
  }

  return (
    <div className="App">
      <header>
        <h1>Chronoquiz</h1>
        <Instructions />
        <InputForm updateFn={setClues} />
        <ScorePanel score={score} />
      </header>
      <main>
        <Clues />
        <Timeline />
      </main>
    </div>
  );
}

export default App;
