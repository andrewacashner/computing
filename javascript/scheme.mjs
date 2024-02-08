#!/usr/bin/env node
// Scheme interpreter in Javascript
// Andrew A. Cashner 2024/02/01

import Node from "./tree.mjs";

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

const Scheme = {

  cons: (a, b) => new ListItem(a, b),

  car: item => item.data,

  cdr: item => item.next,

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
        return reverseDo(Scheme.cdr(oldLs), Scheme.cons(Scheme.car(oldLs), newLs));
      }
    }
    return reverseDo(ls, null);
  },

  map: function (ls, fn) {
    function mapDo(oldLs, newLs) {
      if (!oldLs) {
        return Scheme.reverse(newLs);
      } else {
        return mapDo(Scheme.cdr(oldLs), Scheme.cons(fn(Scheme.car(oldLs)), newLs));
      }
    }
    return mapDo(ls, null);
  },

  parse: function(str) {
    let tree;

    function addToTree(tree, node) {
      if (!tree) {
        tree = node;
      } else {
        tree.appendChild(node);
      }
      return tree;
    }

    const SyntaxMode = {
      WORD: 1,
      EXPR: 2,
      OUTER: 3
    }

    const isWhiteSpace = c => /\s/.test(c);
    const OPEN = "(";
    const CLOSE = ")";

    // Split a string into arguments;
    // Arguments can be space-delimited words OR s-expressions delimited with
    // parentheses;
    // Return an array of strings containing either the words or the
    // parenthetical expressions; do not parse within the parentheses.
    function args(str, thisArg = "", tree = null, mode = SyntaxMode.OUTER, exprLevel = 0) {

      if (!str) {
        if (thisArg) {
          let node = new Node(thisArg);
          tree = addToTree(tree, node);
        }
        return tree;
      } else {
        let c = str[0];
        
        const advanceStr = str.slice(1);
        const addCharToArg = thisArg + c;
        const skipChar = thisArg;
        const resetWord = "";
        const addNode = (node) => addToTree(tree, node);
        const copyAndContinue = () => args(advanceStr, addCharToArg, tree, mode, exprLevel);
        const copyAndContinueInMode = (newMode) => args(advanceStr, addCharToArg, tree, newMode, exprLevel);

        if (isWhiteSpace(c)) {
          if (mode == SyntaxMode.EXPR) {
            return copyAndContinueInMode(SyntaxMode.EXPR);

          } else if (mode == SyntaxMode.WORD) {
            let node = new Node(thisArg);
            return args(advanceStr, resetWord, addNode(node), SyntaxMode.OUTER, exprLevel);

          } else return copyAndContinueInMode(SyntaxMode.OUTER);

        } else if (c === OPEN && mode === SyntaxMode.OUTER) {
            return args(advanceStr, addCharToArg, tree, SyntaxMode.EXPR, ++exprLevel);

        } else if (c === CLOSE && mode === SyntaxMode.EXPR) {
            if (exprLevel > 1) {
              return args(advanceStr, addCharToArg, tree, SyntaxMode.EXPR, --exprLevel);

            } else {
              let node = args(thisArg.slice(1, -1)); // remove parens
              node = node ?? new Node(thisArg);
              return args(advanceStr, resetWord, addNode(node), SyntaxMode.OUTER, --exprLevel);
            }

        } else {
          if (mode === SyntaxMode.OUTER) {
            return copyAndContinueInMode(SyntaxMode.WORD);

          } else return copyAndContinue();
        }
      }
    }

    try {
      if (str.startsWith("(") && str.endsWith(")")) {
        let inner = str.slice(1, -1);
        let argTree = args(inner);
        console.log(argTree);
        return argTree;
      } else {
        throw "Syntax error";
      }
    } catch(e) {
      console.error(e);
      return undefined;
    }
  },

  eval: function (str) {
    
    function applyFn(node) {
      let fn = "";
      let args = [];
      let value;
      if (node && node.child) {
        fn = node.data;

        for (let child = node.child; child != null; child = child.sibling) {
          if (node.child.child) {
            let arg = applyFn(node.child);
            args.push(arg);
          } else {
            args.push(child.data);
          }
        }
      }
      try {
        if (fn in Scheme) {
          value = Scheme[fn](...args);
        } else throw `Unknown function '${fn}'`;
      } catch(e) {
        console.error(e);
      } 
      return value;
    }

    try {
      let tree = Scheme.parse(str);
      console.log(`${tree}`);
      return applyFn(tree);
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


