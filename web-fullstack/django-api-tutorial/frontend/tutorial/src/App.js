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
    constructor(username = "", email = "") {
      this.username = username;
      this.email = email;
    }
    get isEmpty() {
      return this.username === "" && this.email === "";
    }
    static blank() {
      return new User();
    }
  }

  let [currentUser, setCurrentUser] = useState(User.blank());
  debug(currentUser);

  let [newUser, setNewUser] = useState(User.blank());
  debug(newUser);

  function authenticate(event) {
    event.preventDefault();
    let username = event.target.username.value;
    let password = event.target.password.value;
    setCurrentUser(new User(username, password));
    debug(currentUser);
  }

  function addUser(event) {
    event.preventDefault(); // TODO how to enable return-to-enter?
    let target = event.target;
    let nextUser = new User(target.username.value, target.email.value);
    setNewUser(nextUser);
  }

  useEffect(() => {
    async function login() {
      try {
        let response = await fetch(`${BACKEND_SERVER}/admin/login/`, {
          method: "POST",
          headers: new Headers({
            "Accept": "application/json",
            "Content-Type": "application/json",
          }),
          body: JSON.stringify(currentUser),
        });
        debug(response);
        if (response.ok) {
          setAuthenticated(true);
        } else {
          throw new Error(`Problem logging in: Response status ${response.status}, ${response.statusText}`);
        }
      } catch (e) {
        console.error(e);
      }
    }
    if (!authenticated && !currentUser.isEmpty) {
      login();
    }
  }, [currentUser, authenticated]);

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
            item.fields.username, item.fields.email));

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
     
    if (authenticated) {
      if (!newUser.isEmpty) {
        registerUser(newUser);
      }
      getUsers();
    }
  }, [newUser, authenticated]); // eslint-disable-line react-hooks/exhaustive-deps

  

  function LoginForm() {
    return(
      <section>
        <h1>Log In</h1>
        <form onSubmit={authenticate}>
          <label htmlFor="username">Username</label>
          <input type="text" name="username" />

          <label htmlFor="password">Password</label>
          <input type="password" name="password" />

          <button type="submit">Submit</button>
        </form>
      </section>
    );
  }

  function UserAdmin() {
    return(
      <section>
        <h1>User Admin</h1>
        <p>Welcome, <strong>{currentUser.username}</strong>!</p>
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

            <label htmlFor="email">Email</label>
            <input type="text" name="email" />

            <button type="submit">Submit</button>
          </form>
        </section> 
      </section>
    );
  }

  return (
    <main>
      { authenticated ? <UserAdmin /> : <LoginForm /> }
    </main>
  );
}

export default App;
