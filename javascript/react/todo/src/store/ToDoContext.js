import { createContext } from "react";

const ToDoContext = createContext({
  get: [],
  set: () => {},
  reset: () => {}
});

export default ToDoContext;

