#!/usr/bin/env node
// Testing Scheme interpreter
// Andrew Cashner, 2024/02/12

const GUILE = true;   /* Show comparison with Guile output */
const NATIVE = false; /* Log native value of result, not toString() */

import { Scheme, compareGuile } from "./scheme.mjs";


let ls = Scheme.list(1, 2, 3);
let newNums = Scheme.map(ls, x => x + 1);
console.log(`map +1 ${ls} => ${newNums}`);

let inputs = [
  [Scheme.cons, ["a", "b"]],
  [Scheme.car, ls],
  [Scheme.cdr, ls],
  [Scheme.reverse, ls],
  [Scheme.list, ["a", "b", "c"]],
  [Scheme.eval, "(cons 1 2)"],
  [Scheme.eval, "(cons (cons 1 2) 3)"],
  [Scheme.eval, "(cons 1 (cons 2 (cons 3 (cons 4 '()))))"], 
  [Scheme.eval, "(list 1 2 3)"],
  [Scheme.eval, "(cons 1 (cons 2 3))"],
  [Scheme.eval, "(append (cons 1 2) (list 3 4))"], // throw error: no function 'append'
  [Scheme.eval, "cons 1 2"], // throw error: no parens
  [Scheme.eval, "(cons 1 2"], // throw error: no parens
  [Scheme.eval, "cons 1 (2)"], // throw error: no parens
  [Scheme.eval, "(list 0 (cons 1 2)"], // throw error: unbalanced parens
  [Scheme.eval, "(cons 1 2)"],
  [Scheme.eval, "(cons 'a '())"],
  [Scheme.eval, "(cons 1 (cons 2 '()))"],
  [Scheme.eval, "(list 1 2)"],
  [Scheme.eval, "(list? ls)"],
  [Scheme.eval, "(pair? (list 1 2))"],
  [Scheme.eval, "(list? (list 1 2))"],
  [Scheme.eval, "(list 1 2)"],
  [Scheme.eval, "(list? (cons 1 2))"],
  [Scheme.eval, "(pair? (cons 1 2))"],
  [Scheme.eval, "(cons 1 '())"],
  [Scheme.eval, "(list? (cons 1 '()))"],
  [Scheme.eval, "(pair? (cons 1 '()))"],
  [Scheme.eval, "(cons 1 2)"],
  [Scheme.eval, "(cons 1 (cons 2 (cons 3 4)))"], 
  [Scheme.eval, "(list 0 (cons 1 2) (cons 3 (cons 4 5)))"],
  [Scheme.eval, "(cons '() '())"],
  [Scheme.eval, "(list '())"],
  [Scheme.eval, "(list 'a '())"],
  [Scheme.eval, "1"], // TODO should return argument
  [Scheme.eval, "'a"], // TODO should return argument without quote
  [Scheme.eval, "'()"], // Should return "()"
  [Scheme.eval, "(cons 1 2))"] // TODO should return error
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
  if (fn === Scheme.eval && result && GUILE) {
    await compareGuile(arg, result.toString());
  }
}
