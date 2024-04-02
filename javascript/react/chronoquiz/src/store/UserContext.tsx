import { createContext } from "react";
import User from "../classes/User";

const UserContext = createContext({
  get: () => {},
  set: () => {},
  authenticated: [false, () => {}],
  currentUser:   [new User(), () => {}],
  userToken:     [null,  () => {}]
});

export default UserContext;
