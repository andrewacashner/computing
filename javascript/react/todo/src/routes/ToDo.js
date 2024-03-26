// To Do list
// Andrew Cashner
// 2024/02/23

import { useState, useContext } from "react";
import { useNavigate, useLoaderData } from "react-router-dom";

import UserContext from "../store/UserContext";
import ToDoContext from "../store/ToDoContext";

import ToDoItem from "../classes/ToDoItem";
import ToDoList from "../classes/ToDoList";

import TaskList from "../components/TaskList";
import CheckAllButton from "../components/CheckAllButton";
import NewTaskForm from "../components/NewTaskForm";

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
  
  const userList = useLoaderData();

  const emptyList = () => new ToDoList();
  const startList = userList || emptyList();

  let [items, setItems] = useState(startList);

  let toDoContextValue = {
    get: items,
    set: setItems,
    reset: () => setItems(emptyList())
  }

  const navigate = useNavigate();
  
  if (authenticated) {
    return(
      <main>
        <Welcome />
        <ToDoContext.Provider value={toDoContextValue}>
          <section id="todo">
            <div className="todoList">
              <h1>Tasks</h1>
              <p className="instructions">Add a new task using the form below. Drag to rearrange tasks.</p>
              <TaskList />
              <CheckAllButton />
            </div>
              <NewTaskForm />
          </section>
        </ToDoContext.Provider>
      </main>
    );
  } else {
    navigate("/login");
  }
}

export default ToDo;

const BACKEND_SERVER = "http://127.0.0.1:8000";

export async function loader(user, token) {
  let todo = null;
  if (!user.empty && token) {
    try {
      let response = await fetch(`${BACKEND_SERVER}/todo/`, {
        method: "GET",
        headers: new Headers({
          "Accept": "application/json",
          "Content-Type": "application/json",
          "Authorization": `Token ${token}`,
        }),
      });
      if (response.ok) {
        let json = await response.json();
        console.debug("Received todolist data");

        let items = json.map(i => new ToDoItem({
          task: i.task, 
          deadline: i.deadline, 
          isDone: i.is_done
        }));
        todo = new ToDoList(items);
      } else {
        throw new Error(`Could not access data for user ${user.username}`);
      }
    } catch (e) { 
      console.error(e);
    }
  }

  return todo;
}
