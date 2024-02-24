// To Do list
// Andrew Cashner
// 2024/02/23
//
// TODO possibilities
// Only show Deadline input after new task is entered
// Editing entries
// Rearranging entries
// Use Date for deadline
// Show completed items in separate list (move when completed)
// Sort by deadline
// Nested lists
// Log in and save lists
// Multiple named lists

import "./App.css";
import { useState } from "react";

class ToDoItem {
  task;
  deadline;
  isDone;

  constructor({task, deadline = null, isDone = false}) {
    this.task = task;
    this.deadline = deadline;
    this.isDone = isDone;
    this.id = crypto.randomUUID();
  }

  get doneStatus() {
    return (this.isDone) ? "done" : "notDone";
  }

  toggleDoneStatus() {
    this.isDone = !this.isDone;
  }

  toString() {
    let deadline = (this.deadline) ? ` (${this.deadline})` : "";
    return this.task + deadline;
  }
}

function MakeNewTaskForm(items, setItems) {
//  const [deadlineVisible, setDeadlineVisible] = useState(false);

  return function() {
//    const deadlineVisibility = (deadlineVisible) ? "show" : "hide";
//    const showDeadline = () => setDeadlineVisible(true);
    const deadlineVisibility = "show";
    const showDeadline = () => {};

    function addNewTask (event) {
      event.preventDefault();

      let task = event.target.task.value;

      if (task) {
        let newTask = new ToDoItem({
          task: event.target.task.value, 
          deadline: event.target.deadline.value
        });
        console.log(`Add new task '${newTask.task}'`);

        setItems([...items, newTask]);
      }
      event.target.reset();
    }

    return(
      <form className="newItem" 
            onSubmit={addNewTask} 
            autoComplete="off">
        <div className="newTaskInput">
          <label htmlFor="newTask">New task:</label>
          <input type="text" 
                name="task" 
                id="newTask" 
                onChange={showDeadline} />
        </div>
        <div className={deadlineVisibility} id="deadline">
          <label htmlFor="newDeadline">Deadline (optional):</label>
          <input type="text" name="deadline" id="newDeadline" />
        </div>
        <button action="submit">Add task</button>
      </form>
    );
  }
}

function dragListItem(event) {
  event.dataTransfer.setData("text/uuid", event.target.id);
}

function dragoverListItem(event) {
  event.preventDefault();
  if (event.target.tagName === "LI") {
    event.target.classList.add("gapAbove");
  }
}

function dragleaveListItem(event) {
  event.target.classList.remove("gapAbove");
}

function moveWithinArray(items, fromID, toID) {
 
  console.log(`Move from item ${fromID} to item ${toID}`);
  function insertBefore(array, matchFn, item) {
    let insertPoint = array.findIndex(matchFn);
    let before = array.slice(0, insertPoint);
    let after = array.slice(insertPoint);
    return [...before, item, ...after];
  }

  let itemToMove = items.find(i => i.id === fromID);
  let rest = items.filter(i => i !== itemToMove);

  let newItems = [];
  if (toID === "bottom") {
    newItems = [...rest, itemToMove];
  } else {
    newItems = insertBefore(rest, (i => i.id === toID), itemToMove);
  }
  return newItems;
}

function makeDropListItem(items, setItems) {
  return function(event) {
    event.preventDefault()

    if (items.length > 1) {
      let fromID = event.dataTransfer.getData("text/uuid");

      // Check if item was dropped below the list items, in the extra space we
      // leave at bottom of the ol.todo
      console.log(event.target.className);
      let toID = (event.target.className === "todo") 
                  ? "bottom" : event.target.id;

      if (fromID !== toID) {
        let newItems = moveWithinArray(items, fromID, toID);
        setItems(newItems);
      }
    }
  }
}

function MakeListItems(items, setItems) {

  function ListItem(item) {
    const [itemDone, setItemDone] = useState(false);
    item.isDone = itemDone;

    const toggleDoneStatus = (event) => setItemDone(!itemDone);

    return (
      <li key={item.id}
          id={item.id}
          className={item.doneStatus}
          onClick={toggleDoneStatus}
          draggable="true"
          onDragStart={dragListItem}>{`${item}`}</li>
    );
  }
  let dropListItem = makeDropListItem(items, setItems);

  return function() {
    return(
      <ol className="todo"
          onDragOver={dragoverListItem}
          onDragLeave={dragleaveListItem}
          onDrop={dropListItem}>
        {items.map(ListItem)}
      </ol>
    );
  }
}

function ToDoList() {
  let [items, setItems] = useState([]);
  
  let ListItems = MakeListItems(items, setItems);
  let NewTaskForm = MakeNewTaskForm(items, setItems);

  return(
    <section id="todo">
      <div className="todoList">
        <h1>To Do</h1>
        <ListItems />
      </div>
      <NewTaskForm />
    </section>
  );
}

function App() {
  return(<ToDoList />);
}

export default App;
