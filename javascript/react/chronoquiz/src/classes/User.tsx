import debug from "../lib/debug";
import Fact from "./Fact";
import Timeline from "./Timeline";

interface UserInput {
  username: string,
  email: string,
  password: string 
} 

interface RequestInput {
  url: string, 
  method: string, 
  bodyObject: object,
  token: string 
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

  async request({ url, method, bodyObject, token }: RequestInput): object {

    let fullUrl = `${User.SERVER}/${url}`;

    let authorization = token 
      ? { "Authorization": `Token ${token}` } 
      : null;

    let msg = {
      method: method,
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
    let response = await this.request({
      url: "check_user/", 
      method: "POST", 
      bodyObject: this
    });
    if (response.ok) {
      answer = true;
    } else {
      debug("User not found");
    }
    return answer;
  }

  async register(): boolean {
    let answer = false;
    let response = await this.request({
      url: "register/", 
      method: "POST", 
      bodyObject: this
    });
    if (response.ok) {
      answer = true;
    } else {
      debug("Could not register user");
    }
    return answer;
  }

  async authenticate(): string {
    let token = null;
    let response = await this.request({
      url: "login/", 
      method: "POST", 
      bodyObject: this
    });
    if (response.ok) {
      let json = await response.json();
      token = json.token;
      debug(`Authenticated user ${this.username}`);
    } else {
      debug(`Problem authenticating user ${this.username}: Response status ${response.status}, ${response.statusText}`);
    }
    return token;
  }

  async loadUserTimelineList(token: string = ""): array<string> {
    let list = null;
    let response = await this.request({
      url: "timelines/", 
      method: "POST", 
      bodyObject: this, 
      token: token
    });
    if (response.ok) {
      let json = await response.json();
      list = json;
      debug(`Loaded list of ${json.length} timelines`);
    } else {
      debug(`Problem retrieving quiz list: Server responded ${response.status}, ${response.statusText}`);
    }
    return list;
  }

  async fetchTimeline(id: number, token: string): Timeline {
    let newTimeline = null;

    let response = await this.request({
      url: `timeline-full/${id}/`, 
      method: "GET",
      token: token
    });

    if (response.ok) {
      let json = await response.json();
      debug(json);
      let creator = (json.creator === "") ? this.username : json.creator;

      newTimeline = new Timeline({
        id:           json.id,
        title:        json.title,
        description:  json.description,
        keywords:     Timeline.parseKeywords(json.keywords),
        creator:      creator,
        facts:        json.facts.map(f => Fact.newFromYear(f))
      });
    } else {
      debug(`Problem loading timeline with id ${id}: Server status ${response.status}, ${response.statusText}`);
    }

    return newTimeline;
  }
}
