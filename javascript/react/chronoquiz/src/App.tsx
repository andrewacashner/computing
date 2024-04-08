import React from "react";
import "./App.css";

import { useState } from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";

import Layout from "./components/shared/Layout";
import About from "./routes/About";
import Admin from "./routes/Admin";
import Create from "./routes/Create";
import Choose from "./routes/Choose";
import Chronoquiz from "./routes/Chronoquiz";

import User from "./classes/User";
import Game from "./classes/Game";

import UserContext from "./store/UserContext";
import TimelineContext from "./store/TimelineContext";

function App() {

  let userContext = {
    get:           key => userContext[key][0],
    set:           key => userContext[key][1],
    authenticated: useState(false),
    currentUser:   useState(new User()),
    userToken:     useState(null)
  };

  let timelineContext = {
    get:      key => timelineContext[key][0],
    set:      key => timelineContext[key][1],
    timeline: useState(new Game())
  };

  // TODO user-readable url like this?
  // <Route path="/game/:username/:gameTitle" element={<Chronoquiz />} />
  
  return (
    <UserContext.Provider value={userContext}>
      <TimelineContext.Provider value={timelineContext}>
        <BrowserRouter>
          <Routes>
            <Route path="/" element={<Layout />}>
              <Route path="/about" element={<About />} />
              <Route path="/admin" element={<Admin />} />
              <Route path="/admin/create" element={<Create />} />
              <Route path="/game" element={<Choose />} />
              <Route path="/game/:gameId" element={<Chronoquiz />} />
            </Route>
          </Routes>
        </BrowserRouter>
      </TimelineContext.Provider>
    </UserContext.Provider>
  );
}

export default App;
