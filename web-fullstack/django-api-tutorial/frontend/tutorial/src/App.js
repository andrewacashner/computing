import './App.css';

import { useState, useEffect } from "react";
import UserList from "./components/UserList";

function App() {
  let [users, setUsers] = useState([]);

  useEffect(() => {
    async function getUsers() {
      try {
        let response = await fetch("http://127.0.0.1:8000/users/");
        console.debug(response);

        let json = response.ok ? await response.json() : null;
        console.debug(json);

        if (json) {
          setUsers(json.results);
          console.debug(users);
        } else {
          throw new Error("Empty JSON");
        }
      } catch(e) {
        console.error(e);
      }
    }

    getUsers();
  }, []); // eslint-disable-line react-hooks/exhaustive-deps

  return (
    <main>
      <section>
        <h1>Users</h1>
        <div className="App">
          <UserList users={users} />
        </div>
      </section>
    </main>
  );
}

export default App;
