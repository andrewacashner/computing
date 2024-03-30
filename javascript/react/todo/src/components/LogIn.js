import { useContext } from "react";
import UserContext from "../store/UserContext";
import User from "../classes/User";

export default function LogIn() {
  let userContext = useContext(UserContext);
  let setCurrentUser  = userContext.set("currentUser");
  let setDoLogout     = userContext.set("doLogout");
  let setRegisterNew  = userContext.set("registerNew");


  function login(event) {
    event.preventDefault();
    setCurrentUser(prev => new User({ 
      username: event.target.username.value,
      password: event.target.password.value
    }));
    setDoLogout(false);

    if (event.target.method.value === "register") {
      setRegisterNew(true);
      console.debug("Ready to register new user");
    } else {
      console.debug("Log in");
    }
  }

  //                pattern="^(?=.*\d)(?=.*[a-z])(?=.*[A-Z])(?=.*\W).{8,16}$"

  return(
    <form className="loginForm" onSubmit={login}>
      <div id="username">
        <label htmlFor="username">Username</label>
        <input type="text" name="username" required />
      </div>
      <div id="password">
        <label htmlFor="password">Password</label>
        <p className="instructions">(Minimum 8 characters)</p>
        <input type="password" name="password" required minLength="8" />
      </div>
      <div id="method">
        <label htmlFor="login">Log In</label>
        <input type="radio" name="method" value="login" defaultChecked />

        <label htmlFor="register">Register New User</label>
        <input type="radio" name="method" value="register" />
      </div>
      <button type="submit">Submit</button>
    </form>
  );
}
