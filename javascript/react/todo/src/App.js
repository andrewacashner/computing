// To Do list
// Andrew Cashner
// 2024/02/23

import "./App.css";
import { useState } from "react";
import ToDoContext from "./store/ToDoContext";

import ToDoList from "./classes/ToDoList";

import TaskList from "./components/TaskList";
import CheckAllButton from "./components/CheckAllButton";
import NewTaskForm from "./components/NewTaskForm";

function App() {
  let [items, setItems] = useState(new ToDoList());
  
  const emptyTask = {task: "", deadline: ""};
  let [defaults, setDefaults] = useState(emptyTask);

  let toDoContextValue = {
    items: {
      get: items,
      set: setItems
    },
    form: {
      get: defaults,
      set: setDefaults,
      reset: () => setDefaults(emptyTask)
    }
  }

  return(
    <ToDoContext.Provider value={toDoContextValue}>
      <section id="todo">
        <div className="todoList">
          <h1>To Do</h1>
          <p className="instructions">Add a new task using the form below. Drag to rearrange tasks.</p>
          <TaskList />
          <CheckAllButton />
        </div>
          <NewTaskForm />
      </section>
    </ToDoContext.Provider>
  );
}

export default App;
