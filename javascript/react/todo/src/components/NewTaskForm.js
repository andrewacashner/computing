import { useContext } from "react";
import ToDoContext from "../store/ToDoContext";
import ToDoItem from "../classes/ToDoItem";

function NewTaskForm() {
  let context = useContext(ToDoContext);
  let setItems = context.items.set;
  let draft = context.form.get;

  function addNewTask(event) {
    event.preventDefault();

    let task = event.target.task.value;

    let deadline = event.target.deadline.value;
    if (deadline === "") {
      deadline = null;
    }

    if (task) {
      let newTask = new ToDoItem({task, deadline});
      console.log(`Add new task '${newTask.task}' with deadline '${newTask.deadline}'`);
      setItems(prevItems => prevItems.append(newTask));
    }
    context.form.reset();
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
