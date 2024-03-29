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
import ToDo from "./routes/ToDo";
import LogIn from "./routes/LogIn";

import HttpRequest from "./classes/HttpRequest";
import User from "./classes/User";
import ToDoItem from "./classes/ToDoItem";
import ToDoList from "./classes/ToDoList";

import UserContext from "./store/UserContext";
import ToDoContext from "./store/ToDoContext";

function App() {
  let [authenticated, setAuthenticated] = useState(false);
  let [currentUser, setCurrentUser] = useState(User.blank());
  let [registerNew, setRegisterNew] = useState(false);
  let [doLogout, setDoLogout] = useState(false);

  let startingContext = {
    authenticated: [authenticated, setAuthenticated],
    currentUser: [currentUser, setCurrentUser],
    doLogout: [doLogout, setDoLogout],
    registerNew: [registerNew, setRegisterNew]
  };

  const emptyList = () => new ToDoList();
  let [items, setItems] = useState(emptyList());

  let toDoContextValue = {
    get: items,
    set: setItems,
    reset: () => setItems(emptyList())
  }

  useEffect(() => {
    async function register(user) {
      try {
        let request = new HttpRequest({
          method: "POST",
          url: "register/",
          errorMsg: `Could not register new user ${currentUser.username}`,
          bodyObject: user 
        });
        let response = await request.send();
        if (response.ok) {
          alert(`Welcome, ${currentUser.username}! Please log in with your new password.`);
        } else {
          throw new Error(request.error(response));
        }
      } catch (e) {
        console.error(e);
      } finally {
        setRegisterNew(false);
      }
    }
    if (registerNew && !authenticated && !currentUser.isEmpty) {
      console.debug(`Registering new user ${currentUser.username}`)
      register(currentUser);
    } else {
      console.debug("Not registering new user");
    }
  }, [authenticated, registerNew, currentUser]);

  useEffect(() => {
    async function requestToken(user) {
      let newToken = null;
      try {
        let request = new HttpRequest({
          method: "POST",
          url: "api_token_auth/", 
          errorMsg: `Problem requesting token for user ${user.username}`,
          bodyObject: user 
        });
        let response = await request.send();

        if (response.ok) {
          let json = await response.json();
          newToken = json.token;
          setCurrentUser(prev => new User({...prev, token: newToken}));
          console.debug(`Set token to ${newToken}`);
        } else {
          alert("Unrecognized user");
          throw new Error(request.error(response));
        }
      } catch (e) {
        console.error(e);
      } finally {
        return newToken;
      }
    }

    async function logIn(user, token) {
      let result = false;
      try {
        let request = new HttpRequest({
          method: "POST",
          url: "login/",
          errorMsg: `Could not log in user ${user.username} with those credentials`,
          bodyObject: user,
          authToken: token 
        });
        let response = await request.send();

        if (response.ok) {
          setAuthenticated(true);
          console.debug("User logged in");
          result = true;
        } else {
          alert("Could not log you in with those credentials");
          throw new Error(request.error(response));
        }
      } catch (e) {
        console.error(e);
      } finally {
        return result;
      }
    }

    async function authenticate(user) {
      let result = false;
      try {
        if (!registerNew && !authenticated && user.username && !user.token) {
          console.debug(`Requesting token for user ${user.username}`)
          let token = await requestToken(user);
          //    if (currentUser.token && !registerNew && !authenticated) {
          if (token) {
            console.debug(`Authenticating user ${currentUser.username}`)
            result = await logIn(user, token);
          } else {
            console.debug("Not authenticating");
          }
        } else {
          console.debug("Not requesting token");
        }
      } catch(e) {
        console.error(e);
      } finally {
        return result;
      }
    }

    authenticate(currentUser);

  }, [currentUser.username, registerNew]);

  useEffect(() => {
    async function deauthenticate(user) {
      try {
        let request = new HttpRequest({
          method: "POST",
          url: "logout/",
          errorMsg: `Problem logging out user ${user.username}`,
          bodyObject: user,
          authToken: user.token
        });
        let response = await request.send();
          
        if (response.ok) {
          console.debug(`Logged out user ${currentUser.username}`);
        } else {
          throw new Error(request.error(response));
        } 
      } catch (e) {
        console.error(e);
      } finally {
        setDoLogout(false);
        setCurrentUser(User.blank())
        setAuthenticated(false);
      }
    }
    if (doLogout) {
      deauthenticate(currentUser);
    }
  }, [doLogout, currentUser]);

  let [updatedDB, setUpdatedDB] = useState(false);

  useEffect(() => {
    async function updateDB(user) {
      let todo = null;
        try {
          if (authenticated) {
            let request = new HttpRequest({
              method: "POST",
              url: "todo/",
              errorMsg: `Problem syncing data for user ${user.username}`,
              bodyObject: items,
              authToken: user.token
            });
            let response = await request.send();

            if (response.ok) {
              let json = await response.json();
              console.debug(json);
              setUpdatedDB(true);
            } else {
              throw new Error(request.error(response));
            }
          } 
        } catch (e) { 
          console.error(e);
        } 
    }
    updateDB(currentUser);
  }, [items, authenticated, currentUser]);

  useEffect(() => {
    async function sync(user) {
      let todo = null;
        try {
          if (authenticated) {
            let request = new HttpRequest({
              method: "GET",
              url: "todo/",
              errorMsg: `Problem syncing data for user ${user.username}`,
              authToken: user.token
            });
            let response = await request.send();

            if (response.ok) {
              let json = await response.json();
              console.debug("Synced todolist data");

              let items = json.map(i => new ToDoItem({ id: i.uuid, ...i}));
              todo = new ToDoList(items);
              setItems(todo);
            } else {
              throw new Error(request.error(response));
            }
          } else {
            setItems(emptyList());
          }
        } catch (e) { 
          console.error(e);
        } 
    }
    sync(currentUser);
  }, [authenticated, currentUser, updatedDB]);
  
  const router = createBrowserRouter(
    createRoutesFromElements(
      <Route path="/" element={<Layout />}>
        <Route path="/about" element={<About />} />
        <Route path="/login" element={<LogIn />} />
        <Route path="/todo" element={<ToDo />} />
      </Route>
    )
  );

        // <Route path="/todo" element={<ToDo />} loader={loader} />

  return(
    <UserContext.Provider value={startingContext}>
      <ToDoContext.Provider value={toDoContextValue}>
        <RouterProvider router={router} />
      </ToDoContext.Provider>
    </UserContext.Provider>
  );
}

export default App;


