class User {
  constructor({ 
    username = null, 
    password = null,  
    token = null, 
    authenticated = false } = {}) {

    this.username = username;
    this.password = password;
    this.token = token;
    this.authenticated = authenticated;
  }
}

const BACKEND_SERVER = "http://127.0.0.1:8000";

const STORE = {
  user: new User(),
}


function register(user) {
}

async function requestToken(user) {
  let token = null;
  let msg = {
    method: "POST",
    headers: new Headers({
      "Content-Type": "application/json",
      "Accept": "application/json",
    }),
    body: JSON.stringify(user)
  };
  let response = await fetch(`${BACKEND_SERVER}/api_token_auth/`, msg);

  if (response.ok) {
    let json = await response.json();
    token = json.token;
    console.debug(`Received token ${token}`);
  } else {
    console.debug("Did not receive a token");
  }
  return token;
}

async function authenticate(user) {
  let result = false;
  let msg = {
    method: "POST",
    headers: new Headers({
      "Content-Type": "application/json",
      "Accept": "application/json",
      "Authorization": `Token ${user.token}`,
    }),
    body: JSON.stringify(user),
  };
  let response = await fetch(`${BACKEND_SERVER}/login/`, msg);

  if (response.ok) {
    let json = await response.json();
    console.debug(json);
    result = true;
    console.debug(`Logged in user ${user.username}`);
  } else {
    console.debug(`Could not log in user ${user.username}`);
  }
 
  return result;
}

async function logout() {
  let user = STORE.user;
  let msg = {
    method: "POST",
    headers: new Headers({
      "Content-Type": "application/json",
      "Accept": "application/json",
      "Authorization": `Token ${user.token}`,
    }),
    body: JSON.stringify(user),
  };
  let response = await fetch(`${BACKEND_SERVER}/logout/`, msg);

  if (response.ok) {
    let json = await response.json();
    console.debug(json);
    STORE.user.authenticated = false;
    console.debug(`Logged out user ${user.username}`);
    window.postMessage("authentication change");
  } else {
    console.debug(`Could not log out user ${user.username}`);
  }
}

function welcome(user) {
  return `Welcome, ${user.username}!`
}

function invite() {
  return "Register or Log in";
}

async function login(event) {
  event.preventDefault();
  let username = event.target.username.value;
  let password = event.target.password.value;
  let user = new User({ username, password });
  console.debug(`Received info for user ${STORE.user.username}`);

  let token = await requestToken(user);
  if (token) { 
    user.token = token;

    user.authenticated = await authenticate(user);
    STORE.user = user;

    let panel = document.querySelector("div#login-panel");
    window.postMessage("authentication change");
  }

  let form = document.querySelector("form#login");
  form.reset();
}

  
function showPanel(user) {
  let panel = document.querySelector("div#login-panel");
  let greeting = document.querySelector("p#greeting");
  let logoutButton = document.querySelector("button#logout");
  if (user.authenticated) {
    panel.classList.add("hide");
    greeting.textContent = welcome(user);
    logoutButton.classList.remove("hide");
  } else {
    greeting.textContent = invite();
    panel.classList.remove("hide");
    logoutButton.classList.add("hide");
  }
}

document.addEventListener("DOMContentLoaded", () => {
  let panel = document.querySelector("div#login-panel");

  let form = panel.querySelector("form#login");
  form.addEventListener("submit", login);

  let logoutButton = document.querySelector("button#logout");
  logoutButton.addEventListener("click", logout);

  showPanel(STORE.user);
  window.addEventListener("message", (event) => {
    if (event.data === "authentication change") {
      showPanel(STORE.user);
    }
  });
});
