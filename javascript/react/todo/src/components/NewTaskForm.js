import { ToDoItem } from "../classes/ToDoItem";

function NewTaskForm(props) {
  let items = props.itemState.obj;
  let setItems = props.itemState.fn;
  let formDefaults = props.formState.obj;
  let setFormDefaults = props.formState.fn;

  function addNewTask(event) {
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
        <input type="text" name="task" id="newTask" 
               defaultValue={formDefaults.task} />
      </div>
      <div className="deadlineInput">
        <label htmlFor="newDeadline">Deadline (optional):</label>
        <input type="text" name="deadline" id="newDeadline" 
               defaultValue={formDefaults.deadline} />
        <p className="instructions">Dates in natural language will be converted if possible<br />
        (Examples: <q>Tomorrow at 1pm,</q> <q>A week from Thursday,</q> <q>3/5 at 7am</q>)</p>
      </div>
      <button action="submit">Add task</button>
    </form>
  );
}

export default NewTaskForm;
