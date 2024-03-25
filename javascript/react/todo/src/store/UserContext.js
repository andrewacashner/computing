import { createContext } from "react";

const UserContext = createContext({
  authenticated: [],
  currentUser: [],
  userToken: [],
  doLogout: []
});

export default UserContext;

