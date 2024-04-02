interface UserInput {
  username: string,
  email: string,
  password: string 
} 

export default class User {
  username: string;
  email: string;
  password: string;

  constructor(
    { username = "",
      email    = "",
      password = "" }: UserInput = {}
  ) {
    this.username = username;
    this.email = email;
    this.password = password;
  }

  get isEmpty(): boolean {
    return (this.username === ""
            && this.email === ""
            && this.password === "");
  }

  json(): string {
    return JSON.stringify(this);
  }

  static SERVER = "http://127.0.0.1:8000";

  async exists(): boolean {
    let answer = false;
    let response = await fetch(`${User.SERVER}/check_user/`, {
      method: "POST",
      headers: new Headers({
        "Accept": "application/json",
        "Content-Type": "application/json"
      }),
      body: this.json()
    });
    if (response.ok) {
      answer = true;
    } else {
      console.debug("User not found");
    }
    return answer;
  }

  async register(): boolean {
    let answer = false;
    let response = await fetch(`${User.SERVER}/register/`, {
      method: "POST",
      headers: new Headers({
        "Accept": "application/json",
        "Content-Type": "application/json"
      }),
      body: this.json()
    });
    if (response.ok) {
      answer = true;
    } else {
      console.debug("Could not register user");
    }
    return answer;
  }

  async authenticate(): string {
    let token = null;
    let response = await fetch(`${User.SERVER}/login/`, {
      method: "POST",
      headers: new Headers({
        "Accept": "application/json",
        "Content-Type": "application/json"
      }),
      body: this.json()
    });
    if (response.ok) {
      let json = await response.json();
      token = json.token;
      console.debug(`Authenticated user ${this.username}`);
    } else {
      console.debug(`Problem authenticating user ${this.username}: Response status ${response.status}, ${response.statusText}`);
    }
    return token;
  }
}
