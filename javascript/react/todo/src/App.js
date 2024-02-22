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

}

function ListItem(item) {
  const [itemDone, setItemDone] = useState(false);
  item.isDone = itemDone;

  const toggleDoneStatus = (event) => setItemDone(!itemDone);

  return (
    <li key={item.task.substring(0, 10)} 
        className={item.doneStatus}
        onClick={toggleDoneStatus}>{item.task}</li>
  );
}

function NewTaskForm() {
  const [deadlineVisible, setDeadlineVisible] = useState(false);
  const deadlineVisibility = (deadlineVisible) ? "show" : "hide";

  const showDeadline = () => setDeadlineVisible(true);

//  const [inputItem, setInputItem] = useState(new ToDoItem(""));

  function addNewTask(event) {
    event.preventDefault();
// TODO how to add the new info to a task and add the task to the list?
//    setInputItem(new ToDoItem("raccoon"));
    let newItem = new ToDoItem("raccoon");
    console.log(newItem); // TODO doesn't work.
  }

  return(
    <form id="newItem" onSubmit={addNewTask}>
      <label htmlFor="newTask">New task:</label>
      <input type="text" id="newTask" onChange={showDeadline}/>
      <div className={deadlineVisibility} id="deadline">
        <label htmlFor="newDeadline">Deadline (optional):</label>
        <input type="text" id="newDeadline" />
      </div>
      <button action="submit">Add task</button>
    </form>
  );
}


function TodoList(props) {
  return(
    <div className="todoList">
      <h1>To Do</h1>
      <ol className="todo">
        {props.children.map(ListItem)}
      </ol>
      <NewTaskForm />
    </div>
  );
}

function App() {
  const todo = [
    { task: "Make a to-do list" },
    { task: "Check off items on list" }
  ];
  const todoItems = todo.map(i => new ToDoItem(i));

  return (
    <TodoList>{todoItems}</TodoList>
  );
}


export default App;
