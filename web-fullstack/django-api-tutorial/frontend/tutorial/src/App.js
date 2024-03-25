import './App.css';

import { useState, useEffect } from "react";
import UserList from "./components/UserList";

const BACKEND_SERVER = "http://127.0.0.1:8000";

const DEBUG = true;

function debug(msg) {
  if (DEBUG) {
    console.debug(msg);
  }
}

function App() {

  let [authenticated, setAuthenticated] = useState(false);

  let [users, setUsers] = useState([]);
  debug("users");
  debug(users);


  class User {
    constructor(username = "", password = "") {
      this.username = username;
      this.password = password;
    }
    get isEmpty() {
      return this.username === "" && this.password === "";
    }
    static blank() {
      return new User();
    }
  }

  let [currentUser, setCurrentUser] = useState(User.blank());
  debug(currentUser);

  let [newUser, setNewUser] = useState(User.blank());
  debug(newUser);

  let [doLogout, setDoLogout] = useState(false);
  
  function login(event) {
    event.preventDefault();
    let username = event.target.username.value;
    let password = event.target.password.value;
    setCurrentUser(new User(username, password));
    debug(currentUser);
    setDoLogout(false);
  }

  function addUser(event) {
    event.preventDefault(); // TODO how to enable return-to-enter?
    let target = event.target;
    let nextUser = new User(target.username.value, target.password.value);
    setNewUser(nextUser);
  }

  function logout() {
    setDoLogout(true);
    console.debug("Log out");
  }

  function postRequest(action, user, token = null) {
    let authorization = token ? { "Authorization": `Token ${token}` } : null;
    let request = {
      url: `${BACKEND_SERVER}/auth/${action}/`, 
      msg: {
        method: "POST",
        headers: new Headers({
          "Accept": "application/json",
          "Content-Type": "application/json",
          ...authorization,
        }),
        body: JSON.stringify(user),
      },
      error: response => `Authentication problem with ${action}: Response status ${response.status}, ${response.statusText}`,
    }
    return request;
  }

  let [userToken, setUserToken] = useState();

  useEffect(() => {
    async function requestToken() {
      let token = null;
      try {
        let tokenRequest = postRequest("api_token_auth", currentUser);
        console.debug(tokenRequest);

        let tokenResponse = await fetch(tokenRequest.url, tokenRequest.msg);
        console.debug("Got token response");
        console.debug(tokenResponse);

        if (tokenResponse.ok) {
          console.debug("Token response OK");
          let tokenResponseJson = await tokenResponse.json();
          console.debug(tokenResponseJson);

          let token = tokenResponseJson.token;
          console.debug(token);
          setUserToken(token);
          console.debug(`Set user token to ${token}`);
        } else {
          throw new Error(tokenRequest.error(tokenResponse));
        }
        return token;
      } catch (e) {
        console.error(e);
      }
    }

    async function authenticate() {
      try {
        let request = postRequest("login", currentUser, userToken);

        let response = await fetch(request.url, request.msg);
        debug(response);
        if (response.ok) {
            setAuthenticated(true);
            console.debug(`Authentication status change: authenticated? = ${authenticated}`);
          } else {
            throw new Error(request.error(response));
          }
      } catch(e) {
        console.error(e);
      }
    }
    if (!authenticated && !currentUser.isEmpty) {
      requestToken();
      authenticate();
    }
  }, [currentUser, userToken]);

  useEffect(() => {
    async function deauthenticate() {
      try {
        let request = postRequest("logout", currentUser, userToken);
        let response = await fetch(request.url, request.msg);
        debug(response);
        if (response.ok) {
          setAuthenticated(false);
          setCurrentUser(User.blank());
          setDoLogout(false);
          console.debug(`Logged out user ${currentUser.username}`);
        } else {
          throw new Error(request.error(response));
        }
      } catch(e) {
        console.error(e);
      }
    }
    if (doLogout) {
      deauthenticate();
    }
  }, [doLogout]);

  useEffect(() => {
    async function getUsers() {
      try {
        let response = await fetch(`${BACKEND_SERVER}/users/`);
        debug(response);

        let json = response.ok ? await response.json() : null;
        debug(json);

        if (json) {
          let data = JSON.parse(json);
          debug(data);
          let currentUsers = data.map(item => new User(
            item.fields.username, item.fields.password));

          debug("currentUsers:"); 
          debug(currentUsers);

          setUsers(currentUsers);
        } else {
          throw new Error("Empty JSON");
        }
      } catch(e) {
        console.error(e);
      }
    }
    if (authenticated) {
      getUsers();
    }
  }, [authenticated]);

  useEffect(() => {
    async function registerUser(user) {
      try {
        let response = await fetch(`${BACKEND_SERVER}/users/`, {
          method: "POST",
          headers: new Headers({
            "Accept": "application/json",
            "Content-Type": "application/json",
          }),
          body: JSON.stringify(user),
        });
        debug(response);
        if (!response.ok) {
          throw new Error(`Problem registering new user: Response status ${response.status}, ${response.statusText}`);
        }
      } catch (e) {
        console.error(e);
      }
    }
     
    if (authenticated && !newUser.isEmpty) {
      registerUser(newUser);
    }
  }, [newUser, authenticated]); 


  function LoginForm() {
    function Form() {
      return(
        <section>
          <h1>Log In</h1>
          <form onSubmit={login}>
            <label htmlFor="username">Username</label>
            <input type="text" name="username" />

            <label htmlFor="password">Password</label>
            <input type="password" name="password" />

            <button type="submit">Submit</button>
          </form>
        </section>
      );
    }

    function LogoutButton() {
      return(
        <section>
          <p>Welcome, <strong>{currentUser.username}</strong>!</p>
          <button type="button" onClick={logout}>Log Out</button>
        </section>
      );
    }

    return(authenticated ? <LogoutButton /> : <Form />);
  }

  function UserAdmin() {
    return(
      <section>
        <h1>User Admin</h1>
        <section>
          <h1>Users</h1>
          <div className="App">
            { users ? <UserList users={users} /> : null }
          </div>
        </section>
        <section>
          <h1>Add a User</h1>
          <form onSubmit={addUser}>
            <label htmlFor="username">Username</label>
            <input type="text" name="username" />

            <label htmlFor="password">Email</label>
            <input type="text" name="password" />

            <button type="submit">Submit</button>
          </form>
        </section> 
      </section>
    );
  }

  return (
    <main>
      <LoginForm />
      { authenticated ? <UserAdmin /> : null }
    </main>
  );
}

export default App;
