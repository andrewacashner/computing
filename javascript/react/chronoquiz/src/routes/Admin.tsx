import { useContext, useState, useEffect } from "react";
import { Link } from "react-router-dom";

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

    let [uploadFile, setUploadFile] = useState({ title: "", file: null });

    function handleUpload(event) {
      event.preventDefault();
      let title = event.target.title.value;
      let infile = event.target.upload.files[0];
      setUploadFile({ title: title, file: infile });
    }

    useEffect(() => {

      // TODO remove duplication (this may not be needed when reading from
      // server)
      function isInputValid(json) {
        return Array.isArray(json) && 
          json.length > 0 &&
          json.every(i => ("date" in i) && ("info" in i));
      }

      async function loadUserTimeline(token, title, file) {
        let text = await file.text();
        let json = JSON.parse(text);
        if (isInputValid(json)) {
          console.debug("Valid input");

          let response = await fetch(`${User.SERVER}/create/`, {
            method: "POST",
            headers: new Headers({
              "Content-Type": "application/json",
              "Accept": "application/json",
              "Authorization": `Token ${token}`
            }),
            body: JSON.stringify({ 
              "title": title, 
              "timeline": json
            })
          });
          if (response.ok) {
            let responseJson = await response.json();
            console.debug(responseJson);
          } else {
            console.debug("Could not create new timeline");
          }
        } else {
          console.debug("Invalid input");
        }
      }
      if (uploadFile.file) {
        loadUserTimeline(userToken, uploadFile.title, uploadFile.file);
      }
    }, [uploadFile]);

    return(
      <main>
        <h1>Your Quizzes</h1>
        <button type="button" onClick={logout}>Log Out</button>
        <TimelineList data={timelineList} hideUser />

        <section id="upload">
          <h2>Upload a New Quiz</h2>
          <p>Upload a quiz in JSON format (see <Link to="/about">instructions</Link>)</p>

          <form id="upload-form" onSubmit={handleUpload}>
            <div>
              <label htmlFor="title">Title of new timeline</label>
              <input type="text" name="title" />
            </div>
            <div>
              <input type="file" name="upload" accept=".json" />
            </div>
            <button type="submit">Upload</button>
          </form>

        </section>
      </main>
    );
  }

  return(
    <>
      { authenticated ? <AdminPanel/> : <LoginForm /> }
    </>
  );
}
