"use strict";

function scmLet(bindings, body) {
//  const localScope = {};
  for (let [name, expr] of bindings) {
//    // add binding to properties of localScope
//    localScope[name] = expr; 

    let search = new RegExp(`\\b${name}\\b`, "gu");

//    // Option 1: replace var with a CALL to the binding in localScope
    //    = evaluation later
//    body = body.replace(search, `localScope[${name}]`); 

//    // Option 2: replace var with VALUE of binding in localScope
    //    = evaluation now
//    body = body.replace(search, localScope[name]); 
    
    // Option 3: simply replace the text in the expression with the value
    body = body.replace(search, expr);
  }
  return body;
}

let expr;

// (let ([a 1] [b 2]) (+ a b))
expr = scmLet([["a", 1], ["b", 2]], "a + b");
console.log(expr);

// (let ([a 1] [b 2]) (cons a b))
expr = scmLet([["a", 1], ["b", 2]], "(cons a b)");
console.log(expr);

