export default class HttpRequest {
  #method;
  #url;
  #errorMsg;
  #bodyObject;
  #authToken;

  constructor({ method, url, errorMsg, bodyObject = null, authToken = null }) {
    this.#method = method;
    this.#url = url;
    this.#errorMsg = errorMsg;
    this.#bodyObject = bodyObject;
    this.#authToken = authToken;
  }

  static BACKEND_SERVER = "http://127.0.0.1:8000";

  fullUrl() {
    return `${HttpRequest.BACKEND_SERVER}/${this.#url}`;
  }

  get error() {
    return function(response) {
      return `${this.#errorMsg}: ` +
             `Response status ${response.status}, ${response.statusText}`;
    }
  }

  headers() {
    let headers = new Headers({
      "Accept": "application/json",
      "Content-Type": "application/json"
    });
    if (this.#authToken) {
      headers.append("Authorization", `Token ${this.#authToken}`);
    }
    return headers;
  }

  body() { 
    return JSON.stringify(this.#bodyObject);
  }

  #GET() {
    let obj = {
      method: "GET",
      headers: this.headers(),
    };
    return obj;
  }

  #POST() {
    let obj = {
      method: "POST",
      headers: this.headers(),
      body: this.body()
    };
    return obj;
  }

  request() {
    switch (this.#method) {
      case("GET"):
        return this.#GET();
      case("POST"):
        return this.#POST();
      default:
        throw new Error(`Unrecognized http request method ${this.#method}`);
    }
  }

  async send() {
    let response = null;
    try {
      response = await fetch(this.fullUrl(), this.request());
    } catch (e) {
      console.error(e);
    }
    return response;
  }


}
