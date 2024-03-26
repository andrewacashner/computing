import "./App.css";
import { useState, useEffect } from "react";
import { 
  createBrowserRouter, 
  createRoutesFromElements,
  Route,
  RouterProvider 
} from "react-router-dom";

import Layout from "./components/shared/Layout";

import About from "./routes/About";
import ToDo, { loader as toDoLoader} from "./routes/ToDo";
import LogIn from "./routes/LogIn";

import User from "./classes/User";

import UserContext from "./store/UserContext";

const BACKEND_SERVER = "http://127.0.0.1:8000";

function App() {
  let [authenticated, setAuthenticated] = useState(false);
  let [currentUser, setCurrentUser] = useState(User.blank());
  let [userToken, setUserToken] = useState("");
  let [doLogout, setDoLogout] = useState(false);

  let startingContext = {
    authenticated: [authenticated, setAuthenticated],
    currentUser: [currentUser, setCurrentUser],
    userToken: [userToken, setUserToken],
    doLogout: [doLogout, setDoLogout]
  };

  function postRequest(action, user, token = null) {
    let authorization = token ? { "Authorization" : `Token ${token}` } : null;
    let request = {
      url: `${BACKEND_SERVER}/${action}/`,
      msg: {
        method: "POST",
        headers: new Headers({
          "Accept": "application/json",
          "Content-Type": "application/json",
          ...authorization,
        }),
        body: JSON.stringify(user),
      },
      error: response => `Authentication problem with ${action}: Response status ${response.stauts}, ${response.statusText}`,
    }
    return request;
  }

  useEffect(() => {
    async function requestToken() {
      try {
        let request = postRequest("api_token_auth", currentUser);
        let response = await fetch(request.url, request.msg);
        if (response.ok) {
          let json = await response.json();
          console.debug(json);
          let token = json.token;
          setUserToken(token);
          console.debug(`Set token to ${token}`);
        } else {
          alert("Unrecognized user");
          throw new Error(request.error(response));
        }
      } catch (e) {
        console.error(e);
      }
    }


    async function authenticate() {
      try {
        let request = postRequest("login", currentUser, userToken);
        let response = await fetch(request.url, request.msg);
        if (response.ok) {
          setAuthenticated(true);
          console.debug("User logged in");
        } else {
          alert("Could not log you in with those credentials");
          throw new Error(request.error(response));
        }
      } catch (e) {
        console.error(e);
      }
    }

    console.debug("Ready to request user token");
    if (!authenticated && !currentUser.isEmpty) {
      console.debug(`Requesting token for user ${currentUser.username}`)
      requestToken();

      console.debug("Ready to authenticate");
      if (userToken && !authenticated && !currentUser.isEmpty) {
        console.debug(`Authenticating user ${currentUser.username}`)
        authenticate();
      }
    }
  }, [currentUser, userToken, doLogout]);

  useEffect(() => {
    async function deauthenticate() {
      try {
        let request = postRequest("logout", currentUser, userToken);
        let response = await fetch(request.url, request.msg);
        if (response.ok) {
          setAuthenticated(false);
          setCurrentUser(User.blank());
          setDoLogout(false);
          setUserToken(null);
          console.debug(`Logged out user ${currentUser.username}`);
        } else {
          throw new Error(request.error(response));
        } 
      } catch (e) {
        console.error(e);
      }
    }
    if (doLogout) {
      deauthenticate();
    }
  }, [doLogout, currentUser, userToken]);

  const loader = () => toDoLoader(currentUser, userToken);
  const router = createBrowserRouter(
    createRoutesFromElements(
      <Route path="/" element={<Layout />}>
        <Route path="/about" element={<About />} />
        <Route path="/login" element={<LogIn />} />
        <Route path="/todo" element={<ToDo />} loader={loader} />
      </Route>
    )
  );


  return(
    <UserContext.Provider value={startingContext}>
      <RouterProvider router={router} />
    </UserContext.Provider>
  );
}

export default App;


