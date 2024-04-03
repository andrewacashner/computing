import { useContext, useState, useEffect } from "react";
import { Link } from "react-router-dom";

import UserContext from "../store/UserContext";
// import TimelineContext from "../store/TimelineContext";

import User from "../classes/User";

export default function Login() {
  let userContext       = useContext(UserContext);
  let currentUser       = userContext.get("currentUser");
  let setCurrentUser    = userContext.set("currentUser");

  let userToken         = userContext.get("userToken");
  let setUserToken      = userContext.set("userToken");

  let authenticated     = userContext.get("authenticated");
  let setAuthenticated  = userContext.set("authenticated");

  //   let timelineContext   = useContext(TimelineContext);
  //   let timeline          = timelineContext.get;
  //   let setTimeline       = timelineContext.set;

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
      let list = await user.loadTimelineList(token);
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
    function timelineLink(item) {
      return(
        <li key={item.id}>
          <Link to={`../game/${item.id}`}>{item.title}</Link>
        </li>
      );
    }

    function Timelines() {
      return(
        <ul>
          {timelineList.map(timelineLink)}
        </ul>
      );
    }

    function logout() {
      setCurrentUser(new User());
      setUserToken(null);
      setAuthenticated(false);
    }

    return(
      <main>
        <h1>Your Quizzes</h1>
        <button type="button" onClick={logout}>Log Out</button>
        <Timelines />
      </main>
    );
  }


  return(
    <>
      { authenticated ? <AdminPanel/> : <LoginForm /> }
    </>
  );
}
