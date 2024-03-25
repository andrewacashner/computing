export default class User {
  constructor(username = "", password = "") {
    this.username = username;
    this.password = password;
  }
 
  get isEmpty() {
    return this.username === "" && this.password === "";
  }
  
  static blank() {
    return new User();
  }
}


