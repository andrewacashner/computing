// To Do list
// Andrew Cashner
// 2024/02/23

import { useContext, useEffect } from "react";
import { useLoaderData } from "react-router-dom";

import UserContext from "../store/UserContext";
import ToDoContext from "../store/ToDoContext";

import HttpRequest from "../classes/HttpRequest";
import ToDoItem from "../classes/ToDoItem";
import ToDoList from "../classes/ToDoList";

import TaskList from "../components/TaskList";
import CheckAllButton from "../components/CheckAllButton";
import NewTaskForm from "../components/NewTaskForm";

function ToDo() {
  let userContext = useContext(UserContext);
  let authenticated = userContext.authenticated[0];
  let currentUser = userContext.currentUser[0];
  let setDoLogout = userContext.doLogout[1];

  let todoContext = useContext(ToDoContext);
  let setItems = todoContext.set;

  function Welcome() {
    return(
      <section>
      <p>Welcome, {currentUser.username}!</p>
      <button type="button" onClick={logout}>Log Out</button>
      </section>
    );
  }

  function logout(event) {
    setDoLogout(true);
    console.debug("Log out");
  }
 
  const userList = useLoaderData();
  
  useEffect(() => {
    if (userList) {
      setItems(userList);
    }
  }, [userList, setItems]);

  if (authenticated) {
    return(
      <main>
        <Welcome />
        <section id="todo">
          <div className="todoList">
            <h1>Tasks</h1>
            <p className="instructions">Add a new task using the form below. Drag to rearrange tasks.</p>
            <TaskList />
            <CheckAllButton />
          </div>
          <NewTaskForm />
        </section>
      </main>
    );
  } 
}

export default ToDo;

export async function loader(user, token, items) {
  let todo = null;
  if (!user.empty && token) {
    try {
      let request = new HttpRequest({
        method: "POST",
        url: "todo/",
        errorMsg: `Problem syncing data for user ${user.username}`,
        bodyObject: items,
        authToken: token
      });
      let response = await request.send();

      if (response.ok) {
        let json = await response.json();
        console.debug("Synced todolist data");

        let items = json.map(i => new ToDoItem({ id: i.uuid, ...i}));
        todo = new ToDoList(items);
      } else {
        throw new Error(request.error(response));
      }
    } catch (e) { 
      console.error(e);
    }
  }

  return todo;
}
