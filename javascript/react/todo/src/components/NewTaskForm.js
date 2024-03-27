import { useContext, useState, useEffect } from "react";
import ToDoContext from "../store/ToDoContext";
import UserContext from "../store/UserContext";

import ToDoItem from "../classes/ToDoItem";
import HttpRequest from "../classes/HttpRequest";

function NewTaskForm() {

  let [newItem, setNewItem] = useState();

  let userContext = useContext(UserContext);
  let currentUser = userContext.currentUser[0];
  let userToken = userContext.userToken[0];

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
        deadline: deadline
      });

      console.log(`Add new task '${newTask.task}' with deadline '${newTask.deadline}'`);
      setItems(prevItems => prevItems.append(newTask));
      setNewItem(newTask);

    }
    event.target.reset();
  }

  useEffect(() => {
    // Send new item to backend
    async function saveNewItem(item, token) {
      try {
        let request = new HttpRequest({
          method: "POST",
          url: "todo/add/",
          errorMsg: `Could not add new task for user ${currentUser.username}`,
          bodyObject: item,
          authToken: token 
        });
        let response = await request.send();
        if (response.ok) {
          let json = await response.json();
          console.log(json);
        } else {
          throw new Error(request.error(response));
        }
      } catch(e) {
        console.error(e);
      }
    }
    if (newItem) {
      saveNewItem(newItem, userToken);
    }
  }, [newItem, currentUser, userToken]);

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
