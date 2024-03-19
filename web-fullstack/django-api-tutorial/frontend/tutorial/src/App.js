import './App.css';

import { useState, useEffect } from "react";
import UserList from "./components/UserList";

function App() {
  let [users, setUsers] = useState([]);

  useEffect(() => {
    fetch("http://127.0.0.1:8000/users/")
    .then(result => result.json())
    .then(json => {
      setUsers(json);
      console.log(users);
    })
    .catch(console.error)
  });

  return (
    <div className="App">
      <UserList users={users} />
    </div>
  );
}

export default App;
