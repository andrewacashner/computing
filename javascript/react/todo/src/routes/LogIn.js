import { useContext } from "react";
import { useNavigate } from "react-router-dom";
import UserContext from "../store/UserContext";
import User from "../classes/User";

export default function LogIn() {
  let context = useContext(UserContext);
  let authenticated = context.authenticated[0];
  let setCurrentUser = context.currentUser[1];
  let setDoLogout = context.doLogout[1];

  function login(event) {
    event.preventDefault();
    let username = event.target.username.value;
    let password = event.target.password.value;
    setCurrentUser(new User(username, password));
    setDoLogout(false);
    console.debug("Log in");
  }

   //                pattern="^(?=.*\d)(?=.*[a-z])(?=.*[A-Z])(?=.*\W).{8,16}$"

  function LoginForm() {
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
        <button type="submit">Log In</button>
      </form>
    );
  }

  const navigate = useNavigate();

  if (authenticated) {
    navigate("/todo");
  } else {
    return(
      <LoginForm />
    );
  }
}
