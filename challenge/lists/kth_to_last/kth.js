// Find kth item from end of singly linked list
// 2024/05/02

class Node {
  constructor(data, next) {
    this.data = data;
    this.next = next;
  }

  toString() {
    let output = `${this.data}`;
    if (this.next) {
      output = "(" + output;
      let current = this.next;
      while (current !== null) {
        output += ` ${current.data}`;
        current = current.next;
      } 

      output += ")";
    } 
    return output;
  }

  static list(...data) {
    let head = null;
    let prev = null;
    for (let d of data) {
      let node = new Node(d, null);
      if (head === null) {
        head = node;
        prev = node;
      } else {
        prev.next = node;
        prev = node;
      }
    }
    return head;
  }

  last() {
    if (this === null) {
      return null;
    } else if (this.next === null) {
      return this.data;
    } else {
      return this.next.last();
    }
  }

  count(n = 1) {
    if (this.next === null) {
      return n;
    } else {
      return this.next.count(n + 1);
    }
  }

  at(index) {
    let thisIndex = 0;
    if (Math.abs(index) >= this.count()) {
      return null;
    }
    if (index < 0) {
      console.debug("Adjusted for negative index");
      index += this.count();
    }
    let current = this;
    while (thisIndex < index && current.next !== null) {
      console.debug(`thisIndex ${thisIndex} vs index ${index}`);
      thisIndex += 1;
      current = current.next;
    }

    return current.data;
  }
}

let data = [1, 2, 3, 4, 5, 6, 7];
let list = Node.list(...data);
console.log(`list ${list}`);
console.log(`list.last() ${list.last()}`);
console.log(`list.count() ${list.count()}`);
console.log(`list.at(1) ${list.at(1)}`);
console.log(`list.at(3) ${list.at(3)}`);
console.log(`list.at(9) ${list.at(9)}`);
console.log(`list.at(-12) ${list.at(-12)}`);
console.log(`list.at(-1) ${list.at(-1)}`);
console.log(`list.at(-3) ${list.at(-3)}`);
console.log(`list.at(-6) ${list.at(-6)}`);
