import React from "react";
import "./App.css";

import { useState } from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";

import Layout from "./components/shared/Layout";
import About from "./routes/About";
import Login from "./routes/Login";
import Chronoquiz from "./routes/Chronoquiz";

import User from "./classes/User";

import UserContext from "./store/UserContext";

function App() {
  let userContext = {
    get: key => userContext[key][0],
    set: key => userContext[key][1],
    authenticated: useState(false),
    currentUser:   useState(new User()),
    userToken:     useState(null)
  }

  return (
    <UserContext.Provider value={userContext}>
      <BrowserRouter>
        <Routes>
          <Route path="/" element={<Layout />}>
            <Route path="/about" element={<About />} />
            <Route path="/login" element={<Login />} />
            <Route path="/chronoquiz" element={<Chronoquiz />} />
          </Route>
        </Routes>
      </BrowserRouter>
    </UserContext.Provider>
  );
}

export default App;
