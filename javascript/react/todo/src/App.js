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
  const [deadlineVisible, setDeadlineVisible] = useState(false);

  return function() {
    const deadlineVisibility = (deadlineVisible) ? "show" : "hide";
    const showDeadline = () => setDeadlineVisible(true);

    function addNewTask (event) {
      event.preventDefault();

      let newTask = new ToDoItem({
        task: event.target.task.value, 
        deadline: event.target.deadline.value
      });
      console.log(`Add new task '${newTask.task}'`);

      setItems([...items, newTask]);
      event.target.reset();
    }

    return(
      <form className="newItem" onSubmit={addNewTask} autoComplete="off">
        <label htmlFor="newTask">New task:</label>
        <input type="text" name="task" id="newTask" onChange={showDeadline}/>
        <div className={deadlineVisibility} id="deadline">
          <label htmlFor="newDeadline">Deadline (optional):</label>
          <input type="text" name="deadline" id="newDeadline" />
        </div>
        <button action="submit">Add task</button>
      </form>
    );
  }
}

function MakeListItems(items) {

  function ListItem(item) {
    const [itemDone, setItemDone] = useState(false);
    item.isDone = itemDone;

    const toggleDoneStatus = (event) => setItemDone(!itemDone);

    return (
      <li key={item.task.substring(0, 10)} 
      className={item.doneStatus}
      onClick={toggleDoneStatus}>{`${item}`}</li>
    );
  }

  return function() {
    return(
      <ol className="todo">
        {items.map(ListItem)}
      </ol>
    );
  }
}

function ToDoList() {
  let [items, setItems] = useState([]);
  
  let ListItems = MakeListItems(items);
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
