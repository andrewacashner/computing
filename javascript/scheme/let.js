//"use strict";

function add(a, b) { return Number(a) + Number(b); }

function scmLet(bindings, body) {
  const localScope = {};
  for (let [name, expr] of bindings) {
    localScope[name] = expr;
  }
  let value;
  with (localScope) {   // TODO not allowed in strict mode
    value = eval(body); // TODO dangerous
  }
  return value;
}

let expr = scmLet([["a", 1], ["b", 2]], "add(a, b)");
console.log(expr);

expr = scmLet([["a", 1], ["b", 2]], "a + b");
console.log(expr);
