import { useState, useEffect } from "react";
import User from "../classes/User.js";

const BACKEND_SERVER = "http://127.0.0.1:8000";

export default function LogIn() {

  let [authenticated, setAuthenticated] = useState(false);
  let [currentUser, setCurrentUser] = useState(User.blank());
  let [userToken, setUserToken] = useState("");
  let [doLogout, setDoLogout] = useState(false);

  function login(event) {
    event.preventDefault();
    let username = event.target.username.value;
    let password = event.target.password.value;
    setCurrentUser(new User(username, password));
    setDoLogout(false);
    console.debug("Log in");
  }

  function logout(event) {
    setDoLogout(true);
    console.debug("Log out");
  }
  
  function postRequest(action, user, token = null) {
    let authorization = token ? { "Authorization" : `Token ${token}` } : null;
    let request = {
      url: `${BACKEND_SERVER}/${action}/`,
      msg: {
        method: "POST",
        headers: new Headers({
          "Accept": "application/json",
          "Content-Type": "application/json",
          ...authorization,
        }),
        body: JSON.stringify(user),
      },
      error: response => `Authentication problem with ${action}: Response status ${response.stauts}, ${response.statusText}`,
    }
    return request;
  }

  useEffect(() => {
    async function requestToken() {
      try {
        let request = postRequest("api_token_auth", currentUser);
        let response = await fetch(request.url, request.msg);
        if (response.ok) {
          let json = await response.json();
          console.debug(json);
          let token = json.token;
          setUserToken(token);
          console.debug(`Set token to ${token}`);
        } else {
          alert("Unrecognized user");
          throw new Error(request.error(response));
        }
      } catch (e) {
        console.error(e);
      }
    }
    

    async function authenticate() {
      try {
        let request = postRequest("login", currentUser, userToken);
        let response = await fetch(request.url, request.msg);
        if (response.ok) {
          setAuthenticated(true);
          console.debug("User logged in");
        } else {
          alert("Could not log you in with those credentials");
          throw new Error(request.error(response));
        }
      } catch (e) {
        console.error(e);
      }
    }

    console.debug("Ready to request user token");
    if (!authenticated && !currentUser.isEmpty) {
      console.debug(`Requesting token for user ${currentUser.username}`)
      requestToken();

      console.debug("Ready to authenticate");
      if (userToken && !authenticated && !currentUser.isEmpty) {
        console.debug(`Authenticating user ${currentUser.username}`)
        authenticate();
      }
    }
  }, [currentUser, userToken]);

  useEffect(() => {
    async function deauthenticate() {
      try {
        let request = postRequest("logout", currentUser, userToken);
        let response = await fetch(request.url, request.msg);
        if (response.ok) {
          setAuthenticated(false);
          setCurrentUser(User.blank());
          setDoLogout(false);
          setUserToken(null);
          console.debug(`Logged out user ${currentUser.username}`);
        } else {
          throw new Error(request.error(response));
        } 
      } catch (e) {
          console.error(e);
      }
    }
    if (doLogout) {
        deauthenticate();
      }
    }, [doLogout, currentUser, userToken]);

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

  function Welcome() {
    return(
      <section>
        <h1>Admin Panel</h1>
        <p>Welcome, {currentUser.username}!</p>
        <button type="button" onClick={logout}>Log Out</button>
      </section>
    );
  }

  return(
    <main>
    { authenticated ? <Welcome /> : <LoginForm /> }
    </main>
  );
}
