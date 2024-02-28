// To Do list
// Andrew Cashner
// 2024/02/23
//
// TODO possibilities
// (rejected) Only show Deadline input after new task is entered
// (done) Rearranging entries
// (done) Button to mark all complete or incomplete
// (done) Button to clear all
// (done) Use Date for deadline
// (done) Put date in span for custom format (e.g. red if past due)
// (done) Show completed items in separate list (move when completed)
// (done) Sort by deadline
// (done) Make sort button inactive when list is already sorted
// (done) Add delete button for each item (shown on hover)
// (done) Add edit button for each item
// (done) Break up into separate class and component files
// (done) Use props consistently for components to pass state
// Type new item directly into list instead of form
// Better layout of entries & dates
// Editing entries in-list
// Nested lists
// Log in and save lists
// Multiple named lists

import "./App.css";
import { useState } from "react";
import { ToDoList } from "./classes/ToDoItem";
import TaskList from "./components/TaskList";
import CheckAllButton from "./components/CheckAllButton";
import NewTaskForm from "./components/NewTaskForm";

function App() {
  let [items, setItems] = useState(new ToDoList());
  let updateItems = (newItems) => setItems(new ToDoList(...newItems));

  let [formDefaults, setFormDefaults] = useState({ task: "", deadline: "" });

  let itemState = {obj: items, fn: updateItems};
  let formState = {obj: formDefaults, fn: setFormDefaults};
  let fullState = {itemState: itemState, formState: formState};

  return(
    <section id="todo">
      <div className="todoList">
        <h1>To Do</h1>
        <p className="instructions">Add a new task using the form below. Drag to rearrange tasks.</p>
        <TaskList {...fullState}  />
        <CheckAllButton itemState={itemState} />
      </div>
      <NewTaskForm {...fullState} />
    </section>
  );
}

export default App;
