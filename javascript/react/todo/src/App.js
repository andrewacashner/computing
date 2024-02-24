// To Do list
// Andrew Cashner
// 2024/02/23
//
// TODO possibilities
// (rejected) Only show Deadline input after new task is entered
// (done) Rearranging entries
// (done) Button to mark all complete or incomplete
// (done) Button to clear all
// Type new item directly into list instead of form
// Editing entries in-list
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

  static doneOrNot(isDone) {
    return (isDone) ? "done" : "notDone";
  }

  static activeOrNot(isDone) {
    return (isDone) ? "inactive" : "active";
  }

  get doneStatus() {
    return ToDoItem.doneOrNot(this.isDone);
  }

  get activeStatus() {
    return ToDoItem.activeOrNot(this.isDone);
  }

  static toggled(item) {
    return new ToDoItem({...item, isDone: !item.isDone});
  }

  toString() {
    let deadline = (this.deadline) ? ` (${this.deadline})` : "";
    return this.task + deadline;
  }
}

function makeNewTaskForm(items, setItems) {

  return function() {
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
                id="newTask" />
        </div>
        <div className="deadlineInput">
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

function isDraggedOverSelf(event) {
  return event.target.id === event.dataTransfer.getData("text/uuid");
}

function isDraggedOverNext(event, items) {
  let dragged = event.dataTransfer.getData("text/uuid");
  let current = event.target.id;
  const getIndex = id => items.findIndex(i => i.id === id);
  return getIndex(current) - getIndex(dragged) === 1;
}

function makeDragoverListItem(items) {
  return function(event) {
    event.preventDefault();
    if (event.target.tagName === "LI" 
      && !isDraggedOverSelf(event) 
      && !isDraggedOverNext(event, items)) {
      event.target.classList.add("gapAbove");
    }
  }
}

function dragleaveListItem(event) {
  event.preventDefault();
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
    console.log("Move item to bottom");
    newItems = [...rest, itemToMove];
  } else {
    console.log("Insert item");
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
      let toID = (event.target.className === "todo") 
                  ? "bottom" : event.target.id;

      if (fromID !== toID && !isDraggedOverNext(event, items)) {
        let newItems = moveWithinArray(items, fromID, toID);
        setItems(newItems);
      }
    }
  }
}

function makeListItems(items, setItems) {

  function updateItemsWithToggledItem(items, item) {
    return function() {
      let toggledItem = ToDoItem.toggled(item);
      console.log(`Marking item as ${toggledItem.doneStatus}`);

      let split = items.indexOf(item);
      let before = items.slice(0, split);
      let after = items.slice(split + 1);
      let newItems = [...before, toggledItem, ...after]
      setItems(newItems);
    }
  }

  function ListItem(item) {
    if (item) {
      let toggleDoneStatus = updateItemsWithToggledItem(items, item);

      return (
        <li key={item.id}
        id={item.id}
        className={`${item.doneStatus} ${item.activeStatus}`}
        onClick={toggleDoneStatus}
        draggable="true"
        onDragStart={dragListItem}
        onDragLeave={dragleaveListItem}>{`${item}`}
        </li>
      );
    }
  }

  let dropListItem = makeDropListItem(items, setItems);
  let dragoverListItem = makeDragoverListItem(items);

  return function() {
    return(
      <ol className="todo"
          onDragOver={dragoverListItem}
          onDrop={dropListItem}>
        {items.map(ListItem)}
      </ol>
    );
  }
}

function makeCheckAllButton(items, setItems) {

  function setAllItemStatus(isDone) {
    let newItems = items.map(item => new ToDoItem({...item, isDone: isDone}));
    setItems(newItems);
  }

  const checkAll = () => setAllItemStatus(true);
  const uncheckAll = () => setAllItemStatus(false);
 
  let checkAllStatus = items.some(i => i.isDone === false);
  let uncheckAllStatus = items.some(i => i.isDone === true);

  function clearAll() {
    setItems([]);
  }

  return function() {
    if (items.length > 0) {
      return(
        <div className="todoControls">
          <button type="button" 
                  onClick={checkAll}
                  className={ToDoItem.activeOrNot(!checkAllStatus)}
          >Mark all as finished</button>
          <button type="button" 
                  onClick={uncheckAll}
                  className={ToDoItem.activeOrNot(!uncheckAllStatus)}
          >Mark all as unfinished</button>
          <button type="button" onClick={clearAll}>Clear all</button>
        </div>
      );
    }
  }
}



function ToDoList() {
  let [items, setItems] = useState([]);
  
  let ListItems = makeListItems(items, setItems);
  let NewTaskForm = makeNewTaskForm(items, setItems);
  let CheckAllButton = makeCheckAllButton(items, setItems);

  return(
    <section id="todo">
      <div className="todoList">
        <h1>To Do</h1>
        <ListItems />
        <CheckAllButton />
      </div>
      <NewTaskForm />
    </section>
  );
}

function App() {
  return(<ToDoList />);
}

export default App;
