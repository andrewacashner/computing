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
// Type new item directly into list instead of form
// Better layout of entries & dates
// Editing entries in-list
// Nested lists
// Log in and save lists
// Multiple named lists

import "./App.css";
import { useState } from "react";
import { ToDoItem, ToDoList } from "./components/ToDoItem";
import Utilities from "./components/Utilities";

function makeNewTaskForm(items, setItems) {

  return function() { 
    function addNewTask (event) {
      event.preventDefault();

      let task = event.target.task.value;

      let deadline = event.target.deadline.value;
      if (deadline === "") {
        deadline = null;
      }

      if (task) {
        let newTask = new ToDoItem({
          task: task,
          deadline: deadline
        });
        console.log(`Add new task '${newTask.task}' with deadline '${newTask.deadline}'`);

        setItems([...items, newTask]);
      }
      event.target.reset();
    }

    return(
      <form className="newItem" onSubmit={addNewTask} autoComplete="off">
        <div className="newTaskInput">
          <label htmlFor="newTask">New task:</label>
          <input type="text" name="task" id="newTask" />
        </div>
        <div className="deadlineInput">
          <label htmlFor="newDeadline">Deadline (optional):</label>
          <input type="text" name="deadline" id="newDeadline" />
          <p className="instructions">Dates in natural language will be converted if possible<br />
          (Examples: <q>Tomorrow at 1pm,</q> <q>A week from Thursday,</q> <q>3/5 at 7am</q>)</p>
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
      let ToDoSpan = item.Span();

      return (
        <li key={item.id}
            id={item.id}
            className={`${item.doneStatus} ${item.activeStatus}`}
            onClick={toggleDoneStatus}
            draggable="true"
            onDragStart={dragListItem}
            onDragLeave={dragleaveListItem}>
          <ToDoSpan />
        </li>
      );
    }
  }

  let dropListItem = makeDropListItem(items, setItems);
  let dragoverListItem = makeDragoverListItem(items);

 
  let [done, notDone] = Utilities.partition(items, (i => i.isDone));

  function ItemsDone() {
    return(done.map(ListItem));
  }

  function ItemsNotDone() {
    return(notDone.map(ListItem));
  }

  return function() {
    return(
      <section id="lists">
        <ol className="todo"
            onDragOver={dragoverListItem}
            onDrop={dropListItem}>
          <ItemsNotDone />
        </ol>
        <ol className="todoDone">
          <ItemsDone />
        </ol>
      </section>
    );
  }
}

function makeCheckAllButton(items, setItems, listSortStatus, setListSortStatus) {

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

  function sortItemsByDate() { 
    let sorted = items.toSortedByDate();
    setItems(sorted);
    setListSortStatus(true);
  }


  return function() {
    if (items.length > 0) {
      setListSortStatus(items.isSorted());
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
          <button type="button" 
                  onClick={sortItemsByDate}
                  className={ToDoItem.activeOrNot(listSortStatus)}
          >Sort by date</button>
          <button type="button" onClick={clearAll}>Clear all</button>
        </div>
      );
    }
  }
}

function TaskList() {
  let [items, setItems] = useState(new ToDoList());
  let [listSortStatus, setListSortStatus] = useState(false);
  const updateItems = (newItems) => setItems(new ToDoList(...newItems));
  
  let ListItems = makeListItems(items, updateItems);
  let NewTaskForm = makeNewTaskForm(items, updateItems);
  let CheckAllButton = makeCheckAllButton(items, updateItems, listSortStatus, setListSortStatus);

  return(
    <section id="todo">
      <div className="todoList">
        <h1>To Do</h1>
        <p className="instructions">Add a new task using the form below. Drag to rearrange tasks.</p>
        <ListItems />
        <CheckAllButton />
      </div>
      <NewTaskForm />
    </section>
  );
}

function App() {
  return(<TaskList />);
}

export default App;
