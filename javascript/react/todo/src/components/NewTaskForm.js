import { useContext } from "react";
import ToDoContext from "../store/ToDoContext";

import ToDoItem from "../classes/ToDoItem";

function NewTaskForm() {
  let todoContext = useContext(ToDoContext);
  let items = todoContext.get;
  let setItems = todoContext.set;
  let draft = items.draftEntry;

  function addNewTask(event) {
    event.preventDefault();

    let task = event.target.task.value;

    let deadline = event.target.deadline.value;
    if (deadline === "") {
      deadline = null;
    }

    if (task) {
      let newTask = new ToDoItem({
        // Transfer id from draftEntry in case we are editing, so that it will update the DB entry on the backend instead of making a new entry
        id: items.draftEntry.id,         
        task: task,
        deadline: deadline,
        userOrder: items.list.length
      });

       console.log(`Add new task '${newTask.task}' with deadline '${newTask.deadline}' (userOrder ${newTask.userOrder})`);
      setItems(prevItems => prevItems.append(newTask));
    }
    event.target.reset();
  }

  return(
    <form className="newItem" onSubmit={addNewTask} autoComplete="off">
      <div className="newTaskInput">
        <label htmlFor="newTask">New task:</label>
        <input type="text" name="task" id="newTask" 
               defaultValue={draft.task} />
      </div>
      <div className="deadlineInput">
        <label htmlFor="newDeadline">Deadline (optional):</label>
        <input type="text" name="deadline" id="newDeadline" 
               defaultValue={draft.deadline} />
        <p className="instructions">Dates in natural language will be converted if possible<br />
        (Examples: <q>Tomorrow at 1pm,</q> <q>A week from Thursday,</q> <q>3/5 at 7am</q>)</p>
      </div>
      <button action="submit">Add task</button>
    </form>
  );
}

export default NewTaskForm;
