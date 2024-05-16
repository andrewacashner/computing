// Class approach 
class Stack {
  #stack;

  constructor() {
    this.#stack = [];
  }

  push(item) {
    this.#stack.push(item);
    return this;
  }

  pop() {
    let top = this.#stack.pop();
    return top;
  }

  stack() {
    return this.#stack;
  }
}

// Function-as-object (= closure) approach
function makeStack() {
  let stack = [];
  function push(arg) {
    stack.push(arg);
    return stack;
  }
  function pop() {
    let top = stack.pop();
    return top;
  }
  return stack;
}

function push(stack, item) {
  return stack("push", item);
}

function pop(stack) {
  return stack("pop");
}

