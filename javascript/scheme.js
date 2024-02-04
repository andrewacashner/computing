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

const scm = {
  cons: (a, b) => new ListItem(a, b),

  head: item => item.data,

  tail: item => item.next,

  list: function (...items) {
    let head = null;
    if (items.length > 0) {
      head = scm.cons(items[0], scm.list(...items.slice(1)));
    }
    return head;
  },

  reverse: function (ls) {
    function reverseDo(oldLs, newLs) {
      if (!oldLs) {
        return newLs;
      } else {
        return reverseDo(scm.tail(oldLs), scm.cons(scm.head(oldLs), newLs));
      }
    }
    return reverseDo(ls, null);
  },

  map: function (ls, fn) {
    function mapDo(oldLs, newLs) {
      if (!oldLs) {
        return scm.reverse(newLs);
      } else {
        return mapDo(scm.tail(oldLs), scm.cons(fn(scm.head(oldLs)), newLs));
      }
    }
    return mapDo(ls, null);
  },

  eval: function (str) {
    let expr = parenString(str);
    if (!expr) throw "Syntax error";
    let words = expr.split(" ");
    let fn = words.shift();
    let args = [];
    for (let word of words.slice(1)) {
      if (!word.startsWith("(")) {
        args.push(words.shift());
      } else {
    //    let next = parenString(words /* rest */
      }
    }


    return scm[fn](...args);
  }
}

// TODO need to process args as a stack

//
//    let expr = str;
//    if (expr[0] === "(") {
//      if (expr.at(-1) !== ")") {
//        throw "Syntax error";
//        // TODO replace with function that actually checks balanced
//        // parentheses
//      } else {
//        expr = expr.substring(expr.indexOf("(") + 1, expr.lastIndexOf(")"));
//        let words = expr.split(" ");
//
//        let fn = words[0];
//        let args = [];
//
//        // Grab arguments; if one starts with open parenthesis, join the
//        // remainder of the string and look for the close parenthesis and eval
//        // that expression, then continue after the end of the expression
//        for (let i = 1; i < words.length; ++i) {
//          if (words[i].includes("(")) {
//            let nestedEval;
//            for (let j = i; j < words.length; ++j) {
//              if (words[j].includes(")")) {
//                let rest = words.slice(i, j + 1).join(" ");
//                nestedEval = scm.eval(rest);
//                args.push(nestedEval);
//                i = j + 1;
//                break;
//              } 
//            } 
//            if (!nestedEval) throw "Syntax error";
//          } else {
//            args.push(words[i]);
//          }
//        }
//        return scm[fn](...args);
//      } 
//    } else {
//      return expr;
//    }
//  }
}

// // Return an array of substrings within the given delimiters, balanced, at
// // each level up to the depth specified
// // "(Hello (world (!))) => [ 'Hello (world (!))', 'world (1)', '!' ]
// // But this fails on something like "(Hello) (world)" (gives "['Hello)
// // (world']")
// function balancedDelimSubstring(str, open, close, depth = 1, matches = []) {
//   if (!str || depth < 0) {
//     return matches;
//   } else {
//     let inner;
//     let start = str.indexOf(open);
//     let nextParen = str.substring(start).indexOf(open);
//     let end = str.lastIndexOf(close);
//     if (start >= 0 && end >= 0) {
//       let inner = str.slice(start + 1, end);
//       let newMatches = inner ? [...matches, inner] : [...matches];
//       return balancedDelimSubstring(inner, open, close, depth - 1, newMatches);
//     } 
//   }
// }

function stringBetweenBalanced(open, close, str) {
  let inner, start, stop;

  for (let level = 0, i = 0; i < str.length; ++i) {
    if (str[i] === open) {
        ++level;
        if (!start) {
          start = i + 1;
        } 
    } else if (str[i] === close && start) {
      --level;
      if (level === 0) {
        stop = i;
        break;
      } 
    } 
  }

  if (stop) {
    inner = str.substring(start, stop);
  } 
  return inner;
}

function parenString(str) { 
  return stringBetweenBalanced("(", ")", str); 
}

// let pair = scm.cons("a", "b");
// console.log(`pair: ${pair}`);
//  
// let ls = scm.list("a", "b", "c");
// console.log(`ls: ${ls}`);
// 
// let first = scm.head(ls);
// console.log(`first ls: ${first}`);
// 
// let rest = scm.tail(ls);
// console.log(`rest ls: ${rest}`);
// 
// let sl = scm.reverse(ls);
// console.log(`reverse ls: ${sl}`);
// 
// let nums = scm.list(1, 2, 3);
// let newNums = scm.map(nums, x => x + 1);
// console.log(`map +1 ls: ${newNums}`);
 
let newPair = scm.eval("(cons 1 2)");
console.log(`newPair: ${newPair}`);
//  
// let newList = scm.eval("(list 1 2 3)");
// console.log(`newList: ${newList}`);
// 
// let nested = scm.eval("(cons 1 (cons 2 3))");
// console.log(`nested ${nested}`);
// 
// let nested2 = scm.eval("(list 0 (cons 1 2) (cons 3 (cons 4 5)))");
// console.log(`nested2 ${nested2}`);
// 
// // let error2 = scm.eval("(list 0 (cons 1 2 (cons 3 (cons 4 5)))");
// 
// // TODO this should produce an error
// // let error = scm.eval("(list 0 (cons 1 2) (cons 3 (cons 4 5))");
// 
// console.log(balancedDelimSubstring("(Hello)", "(", ")"));
// console.log(balancedDelimSubstring("(Hello (world))", "(", ")"));
// console.log(balancedDelimSubstring("(Hello (world (!)))", "(", ")", 2));
// console.log(balancedDelimSubstring("Hello (world (!))", "(", ")", 2));
// console.log(balancedDelimSubstring("Hello (world", "(", ")"));
// console.log(balancedDelimSubstring("(Hello) (world)", "(", ")"));
// console.log(balancedDelimSubstring("(Hello (or goodbye)) (world)", "(", ")"));
console.log(parenString("Oh (Hello (cruel) (old (world)))"));
console.log(parenString("O) no!(it's (alive)"));
console.log(parenString("fish"));
console.log(parenString("()"));


