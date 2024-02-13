#!/usr/bin/env node
// Testing Scheme interpreter
// Andrew Cashner, 2024/02/12

const GUILE = true;   /* Show comparison with Guile output */
const NATIVE = false; /* Log native value of result, not toString() */

import * as scm from "./scheme.mjs";


let ls = scm.Scheme.list(1, 2, 3);
//let newNums = scm.Scheme.map(ls, x => x + 1);
//console.log(`map +1 ${ls} => ${newNums}`);

let inputs = [
  [scm.Scheme.cons, ["a", "b"]],
  [scm.Scheme.list, ["a", "b", "c"]],
  [scm.Scheme.car, ls],
  [scm.Scheme.cdr, ls],
  [scm.Scheme.reverse, ls],
  [scm.Scheme.map, [ls, x => x + 1]],
  [scm.read, "(cons 1 2)"],
  [scm.read, "(cons (cons 1 2) 3)"],
  [scm.read, "(cons 1 (cons 2 (cons 3 (cons 4 '()))))"], 
  [scm.read, "(list 1 2 3)"],
  [scm.read, "(cons 1 (cons 2 3))"],
  [scm.read, "(append (cons 1 2) (list 3 4))"], // throw error: no function 'append'
  [scm.read, "cons 1 2"], // throw error: no parens
  [scm.read, "(cons 1 2"], // throw error: no parens
  [scm.read, "cons 1 (2)"], // throw error: no parens
  [scm.read, "(list 0 (cons 1 2)"], // throw error: unbalanced parens
  [scm.read, "(cons 1 2)"],
  [scm.read, "(cons 'a '())"],
  [scm.read, "(cons 1 (cons 2 '()))"],
  [scm.read, "(list 1 2)"],
  [scm.read, "(list? ls)"],
  [scm.read, "(pair? (list 1 2))"],
  [scm.read, "(list? (list 1 2))"],
  [scm.read, "(list 1 2)"],
  [scm.read, "(list? (cons 1 2))"],
  [scm.read, "(pair? (cons 1 2))"],
  [scm.read, "(cons 1 '())"],
  [scm.read, "(list? (cons 1 '()))"],
  [scm.read, "(pair? (cons 1 '()))"],
  [scm.read, "(cons 1 2)"],
  [scm.read, "(cons 1 (cons 2 (cons 3 4)))"], 
  [scm.read, "(list 0 (cons 1 2) (cons 3 (cons 4 5)))"],
  [scm.read, "(cons '() '())"],
  [scm.read, "(list '())"],
  [scm.read, "(list 'a '())"],
  [scm.read, "1"], // TODO should return argument
  [scm.read, "'a"], // TODO should return argument without quote
  [scm.read, "'()"], // Should return "()"
  [scm.read, "(cons 1 2))"] // TODO should return error
];

function test(fn, expr) {
  let result;
  if (expr instanceof Array) {
    result = fn.apply(null, expr);
  } else {
    result = fn.call(null, expr);
  }
  return result;
}

for (let [fn, arg] of inputs) {
  let result = test(fn, arg);
  console.log(`${fn.name} ${arg} => ${result}`);
  if (NATIVE) console.log(result);
  if (fn === scm.read && result && GUILE) {
    await scm.compareGuile(arg, result.toString());
  }
}
