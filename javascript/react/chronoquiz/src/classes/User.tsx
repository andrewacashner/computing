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

  async request(
    url: string, 
    method: string, 
    bodyObject: object,
    token: string = null
  ): object {

    let fullUrl = `${User.SERVER}/${url}`;

    let authorization = token 
      ? { "Authorization": `Token ${token}` } 
      : null;

    let msg = {
      method: "POST",
      headers: new Headers({
        "Content-Type": "application/json",
        "Accept": "application/json",
        ...authorization
      }),
      body: JSON.stringify(bodyObject)
    }

    let response = await fetch(fullUrl, msg);
    return response;
  }

  async exists(): boolean {
    let answer = false;
    let response = await this.request("check_user/", "POST", this);
    if (response.ok) {
      answer = true;
    } else {
      console.debug("User not found");
    }
    return answer;
  }

  async register(): boolean {
    let answer = false;
    let response = await this.request("register/", "POST", this);
    if (response.ok) {
      answer = true;
    } else {
      console.debug("Could not register user");
    }
    return answer;
  }

  async authenticate(): string {
    let token = null;
    let response = await this.request("login/", "POST", this);
    if (response.ok) {
      let json = await response.json();
      token = json.token;
      console.debug(`Authenticated user ${this.username}`);
    } else {
      console.debug(`Problem authenticating user ${this.username}: Response status ${response.status}, ${response.statusText}`);
    }
    return token;
  }

  async loadTimelineList(token: string): array<string> {
    let list = null;
    let response = await this.request("timelines/", "POST", this, token);
    if (response.ok) {
      let json = await response.json();
      list = json;
      console.debug(`Loaded list of ${json.length} timelines`);
    } else {
      console.debug(`Problem retrieving quiz list for ${user.username}: server responded ${response.status}, ${response.statusText}`);
    }
    return list;
  }
}
