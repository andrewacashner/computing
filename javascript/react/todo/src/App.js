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

import HttpRequest from "./classes/HttpRequest";
import User from "./classes/User";
import ToDoList from "./classes/ToDoList";

import UserContext from "./store/UserContext";
import ToDoContext from "./store/ToDoContext";

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

  const emptyList = () => new ToDoList();
  let [items, setItems] = useState(emptyList());

  let toDoContextValue = {
    get: items,
    set: setItems,
    reset: () => setItems(emptyList())
  }

  useEffect(() => {
    async function requestToken() {
      try {
        let request = new HttpRequest({
          method: "POST",
          url: "api_token_auth/", 
          errorMsg: `Problem requesting token for user ${currentUser.username}`,
          bodyObject: currentUser
        });
        let response = await request.send();

        if (response.ok) {
          let json = await response.json();
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
        let request = new HttpRequest({
          method: "POST",
          url: "login/",
          errorMsg: `Could not log in user ${currentUser.username} with those credentials`,
          bodyObject: currentUser,
          authToken: userToken
        });
        let response = await request.send();

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
        let request = new HttpRequest({
          method: "POST",
          url: "logout/",
          errorMsg: `Problem logging out user ${currentUser.username}`,
          bodyObject: currentUser,
          authToken: userToken
        });
        let response = await request.send();
          
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

  const loader = () => toDoLoader(currentUser, userToken, items);
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
      <ToDoContext.Provider value={toDoContextValue}>
        <RouterProvider router={router} />
      </ToDoContext.Provider>
    </UserContext.Provider>
  );
}

export default App;


