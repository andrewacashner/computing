import { useContext, useState, useEffect } from "react";

import UserContext from "../store/UserContext";
import User from "../classes/User";
import TimelineList from "../components/TimelineList";

export default function Login() {
  let userContext = useContext(UserContext);
  let [currentUser,   setCurrentUser]   = userContext.currentUser;
  let [userToken,     setUserToken]     = userContext.userToken;
  let [authenticated, setAuthenticated] = userContext.authenticated;

  let [timelineList, setTimelineList] = useState([]);

  function login(event) {
    event.preventDefault();

    let username = event.target.username.value;
    let email    = event.target.email.value;
    let password = event.target.password.value;

    setCurrentUser(new User({
      username: username,
      email: email,
      password: password
    }));
  }

  useEffect(() => {
    async function doAuthenticate(user) {
      let exists = await user.exists();
      if (exists) {
        let token = await user.authenticate();
        if (token) {
          setUserToken(token);
          setAuthenticated(true);
        }
      } else {
        if (window.confirm("User not found. Register new user with these credentials?")) {
          console.debug("Ready to register");
          let test = await user.register();
          if (test) {
            doAuthenticate(user);
          } 
        } 
      }
    }
    if (currentUser && !currentUser.isEmpty && !currentUser.authenticated) {
      doAuthenticate(currentUser);
    } else {
      console.debug("No user authenticated");
    }

  }, [currentUser, setUserToken, setAuthenticated]);

  useEffect(() => {
    async function loadTimelineList(user, token) {
      let list = await user.loadUserTimelineList(token);
      if (list) {
        setTimelineList(list);
      }
    }

    if (authenticated) {
      loadTimelineList(currentUser, userToken);
    } else {
      setTimelineList([]);
    }
  }, [authenticated, currentUser, userToken]);

  function LoginForm() {
    return(
      <main>
        <h1>Log In</h1>
        <form id="login" onSubmit={login}>
          <div className="formInputBlock">
            <div className="formItem">
              <label htmlFor="username">Username</label>
              <input type="text" name="username" />
            </div>
            <div className="formItem">
              <label htmlFor="email">Email</label>
              <input type="email" name="email" />
            </div>
            <div className="formItem">
              <label htmlFor="password">Password</label>
              <input type="password" name="password" />
            </div>
          </div>
          <button type="submit">Log In</button>
        </form>
      </main>
    );
  }

  function AdminPanel() {
    function logout() {
      setCurrentUser(new User());
      setUserToken(null);
      setAuthenticated(false);
    }

    return(
      <main>
        <h1>Your Quizzes</h1>
        <button type="button" onClick={logout}>Log Out</button>
        <TimelineList data={timelineList} />
      </main>
    );
  }

  return(
    <>
      { authenticated ? <AdminPanel/> : <LoginForm /> }
    </>
  );
}
