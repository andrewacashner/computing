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

class Scheme {

  static cons = (a, b) => new ListItem(a, b);

  static car = item => item.data;

  static cdr = item => item.next;

  static list = function (...items) {
    let head = null;
    if (items.length > 0) {
      head = Scheme.cons(items[0], Scheme.list(...items.slice(1)));
    }
    return head;
  }

  static reverse = function (ls) {
    function reverseDo(oldLs, newLs) {
      if (!oldLs) {
        return newLs;
      } else {
        return reverseDo(Scheme.cdr(oldLs), Scheme.cons(Scheme.car(oldLs), newLs));
      }
    }
    return reverseDo(ls, null);
  }

  static map = function (ls, fn) {
    function mapDo(oldLs, newLs) {
      if (!oldLs) {
        return Scheme.reverse(newLs);
      } else {
        return mapDo(Scheme.cdr(oldLs), Scheme.cons(fn(Scheme.car(oldLs)), newLs));
      }
    }
    return mapDo(ls, null);
  }

  static #parse = function(str) {

    // Split a string into arguments;
    // Arguments can be space-delimited words OR s-expressions delimited with
    // parentheses;
    // Return an array of strings containing either the words or the
    // parenthetical expressions; do not parse within the parentheses.
    function args(str) {
      let args = [];
      let thisArg = "";
      let inWord = false;
      let inExpr = false;
      let exprLevel = 0;

      for (let c of str) {
        if (/\s/.test(c)) {
          if (inExpr) {
            thisArg += c;
          } else if (inWord) {
            args.push(thisArg);
            inWord = false;
            thisArg = "";
          } else {
            continue;
          }
        } else if (c === "(" && !inWord) {
          inExpr = true;
          inWord = false;
          thisArg += c;
          ++exprLevel;
        } else if (c === ")" && inExpr) {
          --exprLevel;
          inWord = false;
          thisArg += c;
          if (exprLevel === 0) {
            args.push(thisArg);
            inExpr = false;
            thisArg = "";
          }
        } else {
          if (!inWord && !inExpr) {
            inWord = true;
          }
          thisArg += c;
        }
      }
      if (thisArg) {
        args.push(thisArg);
      }
      return args;
    }

    try {
      if (str.startsWith("(") && str.endsWith(")")) {
        let inner = str.slice(1, str.length - 1);
        let argStrings = args(inner);
//        console.log(argStrings);

        let parsed = argStrings.map((arg) => {
          if (arg.includes("(") && arg.includes(")")) {
            return Scheme.#parse(arg);
          } else {
            return arg; 
          }
        });
        return parsed;
      } else {
        throw "Syntax error";
      }
    } catch(e) {
      console.error(e);
      return undefined;
    }
  }

  static eval = function (str) {
    
    function applyFn(elements) {
      let value;
      if (elements.length > 1) {
        let fn = elements[0];
        let args = elements.slice(1);
        try {
          if (fn in Scheme) {
            value = Scheme[fn](...args);
          } else throw `Unknown function '${fn}'`;
        } catch(e) {
          console.error(e);
        }
      } else {
        value = elements;
      }
      return value;
    }

    try {
      let elements = Scheme.#parse(str);
      if (elements) {
        let args = [];
        for (let el of elements) {
          let newArg = (el instanceof Array) ? applyFn(el) : el;
          args.push(newArg);
        }
        return applyFn(args);
      }
    } catch(e) {
      console.error(e);
    }
  }
}

function test(fn, expr) {
  let result = fn.call(null, expr);
  console.log(`${fn.name} ${expr} => ${result}`);
  return result;
}

// let ls = Scheme.list(1, 2, 3);

let inputs = [
//  [Scheme.cons, ["a", "b"]],
//  [Scheme.car, ls],
//  [Scheme.cdr, ls],
//  [Scheme.reverse, ls],
//  [Scheme.list, ["a", "b", "c"]],
  [Scheme.eval, "(cons 1 2)"],
  [Scheme.eval, "(cons (cons 1 2) 3)"],
//  [Scheme.eval, "(list 1 2 3)"],
//  [Scheme.eval, "(cons 1 (cons 2 3))"],
//  [Scheme.eval, "(list 1 2"],
//  [Scheme.eval, "(list 0 (cons 1 2) (cons 3 (cons 4 5)))"], //TODO wrong
//  [Scheme.eval, "(list 0 (cons 1 2)"], // TODO wrong
//  [Scheme.eval, "(append (cons 1 2) (list 3 4))"]
];

for (let [fn, arg] of inputs) {
  test(fn, arg);
}

// let newNums = Scheme.map(ls, x => x + 1);
// console.log(`map +1 ${ls} => ${newNums}`);

// TODO didn't work
// function args(str, argList = [], thisArg = "", 
//   state = { inWord: false, inExpr: false, exprLevel: 0}) {
//  
//   const isWhitespace = (c) => /\s/.test(c);
//   
//   const exprOpen = "(";
//   const exprClose = ")";
// 
//   const isExprStart = (c) => c === exprOpen && !state.inWord;
// 
//   const isExprEnd = (c) => c === ")" && state.inExpr;
// 
// //  console.log(`str '${str}', argList '${argList}'`);
// 
//   if (!str) {
//     if (thisArg) {
//       argList = [...argList, thisArg];
//     } 
//     return argList;
//   } else {
//     let current = str[0];
//     if (isWhitespace(current)) {
//       if (state.inExpr) {
//         return args(str.slice(1), argList, thisArg + current, state);
//       } else if (state.inWord) {
//         return args(str.slice(1), [...argList, thisArg], "", 
//           {inWord: false, ...state});
//       }
//     } else if (isExprStart(current)) {
//       return args(str.slice(1), argList, thisArg + current,
//         { inWord: false, 
//           inExpr: true,
//           exprLevel: state.exprLevel + 1
//         });
//     } else if (isExprEnd(current)) {
//       if (state.exprLevel === 0) {
//         return args(str.slice(1), [...argList, thisArg], "", {
//           inWord: false,
//           inExpr: false,
//           exprLevel: state.exprLevel - 1
//         });
//       } else {
//         return args(str.slice(1), argList, thisArg + current, 
//           { exprLevel: state.exprLevel - 1, 
//             inWord: false, 
//             ...state});
//       }
//       return args(str.slice(1), argList, thisArg, state);
//     } else {
//       if (!state.inWord && !state.inExpr) {
//         state.inWord = true;
//       }
//       return args(str.slice(1), argList, thisArg + current, state);
//     }
//   }
// }
