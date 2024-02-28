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

function makeNewTaskForm(items, setItems, formDefaults, setFormDefaults) {

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
      setFormDefaults({task: "", deadline: ""});
      event.target.reset();
    }

    return(
      <form className="newItem" onSubmit={addNewTask} autoComplete="off">
        <div className="newTaskInput">
          <label htmlFor="newTask">New task:</label>
          <input type="text" 
                 name="task" 
                 id="newTask" 
                 defaultValue={formDefaults.task} />
        </div>
        <div className="deadlineInput">
          <label htmlFor="newDeadline">Deadline (optional):</label>
          <input type="text" 
                 name="deadline" 
                 id="newDeadline" 
                 defaultValue={formDefaults.deadline} />
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

function makeListItems(items, setItems, setFormDefaults) {

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
    let [isButtonShown, setIsButtonShown] = useState(false);
    let listButtonVisibility = (isButtonShown) ? "show" : "hide";

    function showButton() {
      setIsButtonShown(true);
    }

    function hideButton() {
      setIsButtonShown(false);
    }

    function deleteItem(event) {
      setItems(items.filter(i => i !== item));
      console.log(`Deleting item (task: ${item.task})`);
      event.stopPropagation();
    }

    function editItem(event) {
      setFormDefaults({
        task: item.task,
        deadline: item.deadline
      });
      setItems(items.filter(i => i !== item));
      event.stopPropagation();
    }

    if (item) {
      let toggleDoneStatus = updateItemsWithToggledItem(items, item);
      let ToDoSpan = item.Span();

      // Cancellation X is U+1F5D9
      // Edit pencil is U+1F589
      return (
        <li key={item.id}
            id={item.id}
            className={`${item.doneStatus} ${item.activeStatus}`}
            onClick={toggleDoneStatus}
            draggable="true"
            onDragStart={dragListItem}
            onDragLeave={dragleaveListItem}
            onMouseEnter={showButton}
            onMouseLeave={hideButton}>
          <ToDoSpan />
          <button type="button" className={listButtonVisibility}
                  onClick={editItem}>🖉</button>
          <button type="button" className={listButtonVisibility} 
                  onClick={deleteItem}>🗙</button>
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

  function sortItemsByDate() { 
    setItems(items.toSortedByDate());
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
          <button type="button" 
                  onClick={sortItemsByDate}
                  className={ToDoItem.activeOrNot(items.isSorted())}
          >Sort by date</button>
          <button type="button" onClick={clearAll}>Clear all</button>
        </div>
      );
    }
  }
}

function TaskList() {
  let [items, setItems] = useState(new ToDoList());
  let updateItems = (newItems) => setItems(new ToDoList(...newItems));

  let [formDefaults, setFormDefaults] = useState({ task: "", deadline: "" });

  let ListItems = makeListItems(items, updateItems, setFormDefaults);
  let NewTaskForm = makeNewTaskForm(items, updateItems, formDefaults, setFormDefaults);
  let CheckAllButton = makeCheckAllButton(items, updateItems);

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
