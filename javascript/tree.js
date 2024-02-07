// LCRS binary tree
// Andrew Cashner, 2024/02/07

"use strict";

class Node {
  constructor(data, child = null, sibling = null) {
    this.data = data;
    this.child = child;
    this.sibling = sibling;
  }

  lastChild() {
    return (this.child.child) ? this.child.lastChild() : this.child;
  }

  lastSibling() {
    return (this.sibling.sibling) ? this.sibling.lastSibling() : this.sibling;
  }

  lastNextGen() {
    return this.child.lastSibling();
  }

  appendChild(node) {
    if (this.child) {
      this.child.appendSibling(node);
    } else {
      this.child = node;
    }
    return this;
  }

  appendSibling(node) {
    if (this.sibling) {
      this.sibling.appendSibling(node);
    } else {
      this.sibling = node;
    }
    return this;
  }

  toString(indent = 0) {
    const spaces = " ".repeat(indent); 
    const shiftWidth = 2;

    let open = spaces + `<node data="${this.data}"`;
    if (this.child) {
      open += ">";
    }

    let close = this.child ? spaces + "</node>" : " />";

    let child = "";
    if (this.child) {
      child = "\n" + this.child.toString(indent + shiftWidth) + "\n";
    }

    let sibling = "";
    if (this.sibling) {
      sibling = "\n" + this.sibling.toString(indent);
    }

    return open + child + close + sibling;
  }
}

let andrew = new Node("Andrew");
let matt = new Node("Matt", andrew, null);
let ben = new Node("Ben");
let joy = new Node("Joy");
let mom = new Node("Ann L.", andrew);
let shoshana = new Node("Shoshana", mom);
let steve = new Node("Steve");
let david = new Node("David");
let ed = new Node("Ed");

andrew.child = ben;
shoshana.appendSibling(steve);
ed.appendChild(shoshana);
ed.appendChild(david)
andrew.appendChild(joy);

let laura = 
  new Node("Laura", 
    new Node("Randy", 
      matt,
      new Node("Terry", 
        new Node("Kyle"), 
        null)),
    null);

let lynn = new Node("Lynn");
let joel = new Node("Joel");
let nathan = new Node("Nathan");
let judson = new Node("Judson");
lynn.appendChild(joel.appendSibling(nathan)).appendChild(judson);

let tests = [
  andrew.data,
  andrew.child,
  andrew.sibling,
  andrew.toString(),
  matt.toString(),
  `${shoshana}`,
  `${ed}`,
  `${laura}`,
  `${lynn}`,
  lynn.lastNextGen()
];
tests.forEach(fn => console.log(fn));
