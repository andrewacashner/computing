// To Do list
// Andrew Cashner
// 2024/02/23

import { useContext } from "react";

import UserContext from "../store/UserContext";

import TaskList from "./TaskList";
import CheckAllButton from "./CheckAllButton";
import NewTaskForm from "./NewTaskForm";

function ToDo() {
  let userContext = useContext(UserContext);
  let authenticated = userContext.authenticated[0];
  let currentUser = userContext.currentUser[0];
  let setDoLogout = userContext.doLogout[1];

  function Welcome() {
    return(
      <section>
      <p>Welcome, {currentUser.username}!</p>
      <button type="button" onClick={logout}>Log Out</button>
      </section>
    );
  }

  function logout(event) {
    setDoLogout(true);
    console.debug("Log out");
  }
 
  if (authenticated) {
    return(
      <main>
        <Welcome />
        <section id="todo">
          <div className="todoList">
            <h1>Tasks</h1>
            <p className="instructions">Add a new task using the form below. Drag to rearrange tasks.</p>
            <TaskList />
            <CheckAllButton />
          </div>
          <NewTaskForm />
        </section>
      </main>
    );
  } 
}

export default ToDo;
