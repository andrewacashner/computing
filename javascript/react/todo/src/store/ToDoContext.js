import { createContext } from "react";

const ToDoContext = createContext({
  items: {
    get: [],
    set: () => {}
  },
  form: {
    get: {task: "", deadline: ""},
    set: () => {},
    reset: () => {}
  }
});

export default ToDoContext;

