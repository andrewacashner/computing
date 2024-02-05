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
    let str = this.data;
    if (this.next) {
      str += " " + this.next.toString();
    }
    return `(${str})`;
  }
}

class Outline {

  constructor(level = 0, start = this.#unset, end = this.#unset) {
    this.level = level;
    this.start = start;
    this.end = end;
  }

  #unset = -1;

  isStartSet() { return this.start !== this.#unset; }
  isEndSet() { return this.end !== this.#unset; }

  isComplete() { return this.isStartSet() && this.isEndSet(); }

  within(str) {
    return str.slice(this.start, this.end + 1);
  }
}

const Scheme = {
  cons: (a, b) => new ListItem(a, b),

  head: item => item.data,

  tail: item => item.next,

  list: function (...items) {
    let head = null;
    if (items.length > 0) {
      head = Scheme.cons(items[0], Scheme.list(...items.slice(1)));
    }
    return head;
  },

  reverse: function (ls) {
    function reverseDo(oldLs, newLs) {
      if (!oldLs) {
        return newLs;
      } else {
        return reverseDo(Scheme.tail(oldLs), Scheme.cons(Scheme.head(oldLs), newLs));
      }
    }
    return reverseDo(ls, null);
  },

  map: function (ls, fn) {
    function mapDo(oldLs, newLs) {
      if (!oldLs) {
        return Scheme.reverse(newLs);
      } else {
        return mapDo(Scheme.tail(oldLs), Scheme.cons(fn(Scheme.head(oldLs)), newLs));
      }
    }
    return mapDo(ls, null);
  },

  eval: function (str) {
//      if (!(sexpr.startsWith("(") && sexpr.endsWith(")"))) {
//        throw "Syntax error";
//      }
    
    let outlines = analyzeExpr(str);
    console.log(outlines);

    let values = "";
    for (let outline of outlines) {
      let sexpr = outline.within(str);
     
      let args = sexpr.slice(1, sexpr.length - 1).split(" ");
      let fn = args.shift();
      
      let value = Scheme[fn](...args);
      console.log(value);
      values += value;
    }
    return values;
  }
}

// TODO you need to account for normal arguments, not just sexpressions, and
// how to plug value of sexpr back into stream of arguments?

function analyzeExpr(str) {
  let args = [];
  let outlines = [];
  let [open, close] = "()";

  for (let level = 0, i = 0; i < str.length; ++i) {
    if (str[i] === open) {
      let outline = new Outline(level, i);
      outlines.push(outline);
      ++level;
    } else if (str[i] === close) {
      --level;
      let outline = outlines.pop();
      if (!(outline.isEndSet() && outline.level === level)) {
        outline.end = i;
        args.push(outline);
      }
    }
  }

  return args;
}

// let input = "(cons 1 2)";
let input = "(cons (cons 1 2) 3)";
console.log(Scheme.eval(input));

// let outlines = sexpr(input);
// console.log(outlines);

// let input = "list 1 2 3";
// console.log(getArgs(input));
// 
// input = "(list 1) 2 3";
// console.log(getArgs(input));
// 
// let input = "(list 1 2 3)";
// console.log(input);
// console.log(getArgs(input));
// 
// input = "(list (list 1 2) 3)";
// console.log(getArgs(input));
// 
// input = "(list (cons 1 2) \'() 3)";
// console.log(input);
// console.log(getArgs(input));

//input = "(list (cons 1 (list \'a \"string\" 2) 4) '() (cons 2 3))";
// let input = "(list (cons 1 (list \'a \"string\" 2) 4) '())";
// console.log(input);
// console.log(getArgs(input));


// let pair = Scheme.cons("a", "b");
// console.log(`pair: ${pair}`);
//  
// let ls = Scheme.list("a", "b", "c");
// console.log(`ls: ${ls}`);
// 
// let first = Scheme.head(ls);
// console.log(`first ls: ${first}`);
// 
// let rest = Scheme.tail(ls);
// console.log(`rest ls: ${rest}`);
// 
// let sl = Scheme.reverse(ls);
// console.log(`reverse ls: ${sl}`);
// 
// let nums = Scheme.list(1, 2, 3);
// let newNums = Scheme.map(nums, x => x + 1);
// console.log(`map +1 ls: ${newNums}`);
//  
// let newPair = Scheme.eval("(cons 1 2)");
// console.log(`newPair: ${newPair}`);
//  
// let newList = Scheme.eval("(list 1 2 3)");
// console.log(`newList: ${newList}`);
// 
// let nested = Scheme.eval("(cons 1 (cons 2 3))");
// console.log(`nested ${nested}`);
// 
// let nested2 = Scheme.eval("(list 0 (cons 1 2) (cons 3 (cons 4 5)))");
// console.log(`nested2 ${nested2}`);
// 
// // let error2 = Scheme.eval("(list 0 (cons 1 2 (cons 3 (cons 4 5)))");
// 
// // TODO this should produce an error
// // let error = Scheme.eval("(list 0 (cons 1 2) (cons 3 (cons 4 5))");
// 
// console.log(balancedDelimSubstring("(Hello)", "(", ")"));
// console.log(balancedDelimSubstring("(Hello (world))", "(", ")"));
// console.log(balancedDelimSubstring("(Hello (world (!)))", "(", ")", 2));
// console.log(balancedDelimSubstring("Hello (world (!))", "(", ")", 2));
// console.log(balancedDelimSubstring("Hello (world", "(", ")"));
// console.log(balancedDelimSubstring("(Hello) (world)", "(", ")"));
// console.log(balancedDelimSubstring("(Hello (or goodbye)) (world)", "(", ")"));
// console.log(parenString("Oh (Hello (cruel) (old (world)))"));
// console.log(parenString("O) no!(it's (alive)"));
// console.log(parenString("fish"));
// console.log(parenString("()"));


