#!/usr/bin/env node
// Scheme interpreter in Javascript
// Andrew A. Cashner 2024/02/01

"use strict";

class ListItem {
  data;
  next;

  constructor(data, next = null) {
    this.data = data;
    this.next = next;
  }

  toString() {
    let str = "(";
    str += this.data;
    if (this.next) {
      str += " " + this.next.toString();
    }
    str += ")";
    return str;
  }
}

const scm = {
  cons: (a, b) => new ListItem(a, b),

  head: item => item.data,

  tail: item => item.next,

  list: function (...items) {
    let head = null;
    if (items.length > 0) {
      head = this.cons(items[0], this.list(...items.slice(1)));
    }
    return head;
  },

  reverse: function (ls) {
    let self = this;
    function reverseDo(oldLs, newLs) {
      if (!oldLs) {
        return newLs;
      } else {
        return reverseDo(self.tail(oldLs), self.cons(self.head(oldLs), newLs));
      }
    }
    return reverseDo(ls, null);
  },

  map: function (ls, fn) {
    let self = this;
    function mapDo(oldLs, newLs) {
      if (!oldLs) {
        return self.reverse(newLs);
      } else {
        return mapDo(self.tail(oldLs), self.cons(fn(self.head(oldLs)), newLs));
      }
    }
    return mapDo(ls, null);
  },

  eval: function (str) {
    let expr = str;
    if (expr[0] === "(") {
      if (expr.at(-1) !== ")") {
        throw "Syntax error";
        // TODO replace with function that actually checks balanced
        // parentheses
      } else {
        expr = expr.substring(expr.indexOf("(") + 1, expr.lastIndexOf(")"));
        let words = expr.split(" ");

        let fn = words[0];
        let args = [];

        // Grab arguments; if one starts with open parenthesis, join the
        // remainder of the string and look for the close parenthesis and eval
        // that expression, then continue after the end of the expression
        for (let i = 1; i < words.length; ++i) {
          if (words[i].includes("(")) {
            let nestedEval;
            for (let j = i; j < words.length; ++j) {
              if (words[j].includes(")")) {
                let rest = words.slice(i, j + 1).join(" ");
                nestedEval = this.eval(rest);
                args.push(nestedEval);
                i = j + 1;
                break;
              } 
            } 
            if (!nestedEval) throw "Syntax error";
          } else {
            args.push(words[i]);
          }
        }
        return this[fn](...args);
      } 
    } else {
      return expr;
    }
  }
}



let pair = scm.cons("a", "b");
console.log(`pair: ${pair}`);

let ls = scm.list("a", "b", "c");
console.log(`ls: ${ls}`);

let first = scm.head(ls);
console.log(`first ls: ${first}`);

let rest = scm.tail(ls);
console.log(`rest ls: ${rest}`);

let sl = scm.reverse(ls);
console.log(`reverse ls: ${sl}`);

let nums = scm.list(1, 2, 3);
let newNums = scm.map(nums, x => x + 1);
console.log(`map +1 ls: ${newNums}`);

let newPair = scm.eval("(cons 1 2)");
console.log(`newPair: ${newPair}`);
 
let newList = scm.eval("(list 1 2 3)");
console.log(`newList: ${newList}`);

let nested = scm.eval("(cons 1 (cons 2 3))");
console.log(`nested ${nested}`);

let nested2 = scm.eval("(list 0 (cons 1 2) (cons 3 (cons 4 5)))");
console.log(`nested2 ${nested2}`);

// let error2 = scm.eval("(list 0 (cons 1 2 (cons 3 (cons 4 5)))");

// TODO this should produce an error
// let error = scm.eval("(list 0 (cons 1 2) (cons 3 (cons 4 5))");

