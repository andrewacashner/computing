import './App.css';

import { useState, useEffect } from "react";
import UserList from "./components/UserList";

const BACKEND_SERVER = "http://127.0.0.1:8000";

function App() {
  let [users, setUsers] = useState([]);
  console.debug("users");
  console.debug(users);

  class User {
    constructor(username = "", email = "") {
      this.username = username;
      this.email = email;
    }
  }


  const emptyUser = () => new User();

  let [newUser, setNewUser] = useState(emptyUser());
  console.debug(newUser);

  function addUser(event) {
    event.preventDefault(); // TODO how to enable return-to-enter?
    let target = event.target;
    let nextUser = new User(target.username.value, target.email.value);
    setNewUser(nextUser);
  }

  useEffect(() => {
    async function getUsers() {
      try {
        let response = await fetch(`${BACKEND_SERVER}/users/`);
        console.debug(response);

        let json = response.ok ? await response.json() : null;
        console.debug(json);

        if (json) {
          let data = JSON.parse(json);
          console.debug(data);
          let currentUsers = data.map(item => new User(
            item.fields.username, item.fields.email));

          console.debug("currentUsers:"); 
          console.debug(currentUsers);

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
        console.debug(response);
        if (!response.ok) {
          throw new Error(`Problem registering new user: Response status ${response.status}, ${response.statusText}`);
        }
      } catch (e) {
        console.error(e);
      }
    }
    

    if (newUser.username) {
      registerUser(newUser);
    }
    getUsers();

  }, [newUser]); // eslint-disable-line react-hooks/exhaustive-deps



  return (
    <main>
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
    </main>
  );
}

export default App;
