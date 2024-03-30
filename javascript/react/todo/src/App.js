import "./App.css";
import { useState, useEffect } from "react";
import { BrowserRouter, Routes, Route } from "react-router-dom";

import Layout from "./components/shared/Layout";

import About from "./routes/About";

import HttpRequest from "./classes/HttpRequest";
import User from "./classes/User";
import ToDoItem from "./classes/ToDoItem";
import ToDoList from "./classes/ToDoList";

import LogIn from "./components/LogIn";
import ToDo from "./components/ToDo";

import UserContext from "./store/UserContext";
import ToDoContext from "./store/ToDoContext";

function App() {
  // Has the current user been authenticated with the backend server?
  let [authenticated, setAuthenticated] = useState(false);

  // Current user, set from LogIn form component, includes authorization token
  let [currentUser, setCurrentUser] = useState(User.blank());

  // Do we need to register a new user?
  let [registerNew, setRegisterNew] = useState(false);

  // Should we logout?
  let [doLogout, setDoLogout] = useState(false);

  // USER CONTEXT (State connected to user authentication)
  // Access value with (e.g.) userContext.get("authenticated")
  // Access setter function with userContext.set("authenticated")
  let userContext = {
    get: field => userContext[field][0],
    set: field => userContext[field][1],
    authenticated:  [authenticated, setAuthenticated],
    currentUser:    [currentUser,   setCurrentUser],
    doLogout:       [doLogout,      setDoLogout],
    registerNew:    [registerNew,   setRegisterNew]
  };


  // TODOLIST CONTEXT (State containing user's todo list)
  const emptyList = () => new ToDoList();
  let [items, setItems] = useState(emptyList());

  let toDoContext = {
    get: items,
    set: setItems,
    reset: () => setItems(emptyList())
  }

  // REGISTER new user 
  // Trigger: 'currentUser' set and 'registerNew' set to true by LogIn form,
  // when user sets their login info on the LogIn form and selects "Register"
  // button 
  //
  // Effect: Post user data to server, set 'registerNew' back to false
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
        window.location.reload();
      }
    }
    if (registerNew && !authenticated && !currentUser.isEmpty) {
      console.debug(`Registering new user ${currentUser.username}`)
      register(currentUser);
    } else {
      console.debug("Not registering new user");
    }
  }, [authenticated, registerNew, currentUser]);

  // REQUEST TOKEN and AUTHENTICATE registered user
  // Trigger: 'currentUser' set by LogIn form when a user enters their login
  // info on the form and selects "Log In" 
  //
  // Effect: Post user data to server to request a token from the server and
  // if successful, save the token: set 'currentUser.token'.
  //
  // Use the token to log in and set 'authenticated' to true if successful
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
        } else {
          alert("Could not log you in with those credentials");
          throw new Error(request.error(response));
        }
      } catch (e) {
        console.error(e);
      } 
    }

    async function authenticate(user) {
      try {
        if (!registerNew && !authenticated && user.username && !user.token) {
          console.debug(`Requesting token for user ${user.username}`)
          let token = await requestToken(user);
          if (token) {
            console.debug(`Authenticating user ${currentUser.username}`)
            await logIn(user, token);
          } else {
            console.debug("Not authenticating");
          }
        } else {
          console.debug("Not requesting token");
        }
      } catch(e) {
        console.error(e);
      } 
    }

    authenticate(currentUser);

  }, [currentUser]);

  // DEAUTHENTICATE user
  // Trigger: 'doLogout' set to true by ToDo component, when user clicks "Log
  // Out" button
  //
  // Effect: Post logout request to server, reset 'doLogout', 'currentUser',
  // and 'authenticated' to initial (false/blank) values
  //
  // TODO do we even need to contact the server for this?
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

  // UPDATE THE BACKEND DATABASE when data changes on the client side
  // Trigger: When 'items' changes (because the user added a new item, edited
  // an item, checked it done or not done, deleted an item, or sorted the
  // list)
  //
  // Effect: Post the current client-side list data to the server. Server will
  // update the database entries to match the client-side data. Set
  // 'updatedDB' to true.
  useEffect(() => {
    async function updateDB(user) {
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

  // UPDATE THE CLIENT-SIDE DATA TO SYNC WITH SERVER-SIDE DATABASE
  // Trigger: When 'updatedDB' is true, indicating the server was just updated
  //
  // Effect: Get current data list from database and set the client side
  // 'items' to match
  //
  // TODO: Wouldn't it be more efficient to combine this with the previous
  // useEffect, so that the client posts updated data to the server and the
  // server responds with its updated state?
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
 
  // TODO Could we do this conditional routing a better way?
  function Panel() {
    if (authenticated) {
      return(<ToDo />);
    } else {
      return(<LogIn />);
    }
  }

  return(
    <UserContext.Provider value={userContext}>
      <ToDoContext.Provider value={toDoContext}>
        <BrowserRouter>
          <Routes>
            <Route path="/" element={<Layout />}>
              <Route path="about" element={<About />} />
              <Route path="todo" element={<Panel />} />
              <Route path="login" element={<Panel />} />
            </Route>
          </Routes>
        </BrowserRouter>
      </ToDoContext.Provider>
    </UserContext.Provider>
  );
}

export default App;


