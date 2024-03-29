export default class User {
  constructor({username = "", password = "", token = ""} = {}) {
    this.username = username;
    this.password = password;
    this.token = token;
  }
 
  get isEmpty() {
    return this.username === "" && this.password === "";
  }

  get isComplete() {
    return this.username && this.password && this.token;
  }
  
  static blank() {
    return new User();
  }
}


