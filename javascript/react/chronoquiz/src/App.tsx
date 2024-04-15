import React from "react";
import "./App.css";

import { useReducer } from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";

import Layout from "./components/shared/Layout";
import About from "./routes/About";
import Admin from "./routes/Admin";
import Choose from "./routes/Choose";
import Chronoquiz from "./routes/Chronoquiz";

import User from "./classes/User";
import UserContext from "./store/UserContext";

const defaultUserState = {
  currentUser: new User(),
  authenticated: false,
  userToken: null
}

function userReducer(state, action) {
  console.debug(action);
  console.debug(state);

  let obj = action.payload;
  let newState = null;

  switch(action.type) {
    case "set":
      newState = { ...state, ...obj };
    break;

    case "user":
      newState = { ...state, currentUser: obj };
    break;

    case "reset":
      newState = defaultUserState;
    break;

    default:
      newState = state;
  }

  return newState;
}

function App() {

  let [userState, dispatchUser] = useReducer(userReducer, defaultUserState);

  let userContext = {
    get: userState,
    set: dispatchUser
  };

  return (
    <UserContext.Provider value={userContext}>
      <BrowserRouter>
        <Routes>
          <Route path="/" element={<Layout />}>
            <Route path="/about" element={<About />} />
            <Route path="/admin" element={<Admin />} />
            <Route path="/game" element={<Choose />} />
            <Route path="/game/:gameId" element={<Chronoquiz />} />
          </Route>
        </Routes>
      </BrowserRouter>
    </UserContext.Provider>
  );
}

export default App;
