// To Do list
// Andrew Cashner
// 2024/02/23

import { useLoaderData } from "react-router-dom";
import { useState } from "react";
import ToDoContext from "../store/ToDoContext";

import ToDoItem from "../classes/ToDoItem";
import ToDoList from "../classes/ToDoList";

import TaskList from "../components/TaskList";
import CheckAllButton from "../components/CheckAllButton";
import NewTaskForm from "../components/NewTaskForm";

function ToDo() {

  const userList = useLoaderData();

  const emptyList = () => new ToDoList();
  const startList = userList || emptyList();

  let [items, setItems] = useState(startList);

  let toDoContextValue = {
    get: items,
    set: setItems,
    reset: () => setItems(emptyList())
  }

  return(
    <main>
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
}

export default ToDo;

export async function loader() {
  try {
    let response = await fetch("./cashner.json");
    if (!response.ok) {
      throw new Error("Could not access user data");
    }

    let json = await response.json();

    let items = json.map(i => new ToDoItem(i));
    let todo = new ToDoList(items);
    return todo;

  } catch(e) {
    console.error(e);
    return null;
  }
}
