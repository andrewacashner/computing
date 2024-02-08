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
    //
    // TODO this is omitting the last arg in a nested expr
    function args(str, thisArg = "", tree = null, mode = SyntaxMode.OUTER, exprLevel = 0) {

      if (!str) {
        if (exprLevel !== 0) {
          throw "Syntax error: Unbalanced parentheses"
        }
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
          if (mode === SyntaxMode.EXPR) {
            return copyAndContinueInMode(SyntaxMode.EXPR);

          } else if (mode === SyntaxMode.WORD) {
            let node = new Node(thisArg);
            return args(advanceStr, resetWord, addNode(node), SyntaxMode.OUTER, exprLevel);

          } else return copyAndContinueInMode(SyntaxMode.OUTER);

        } else if (c === OPEN && mode === SyntaxMode.OUTER) {
            return args(advanceStr, addCharToArg, tree, SyntaxMode.EXPR, ++exprLevel);

        } else if (c === CLOSE && mode === SyntaxMode.EXPR) {
            if (exprLevel > 1) {
              return args(advanceStr, addCharToArg, tree, SyntaxMode.EXPR, --exprLevel);

            } else {
              let node = Scheme.parse(addCharToArg); 
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
        str = str.slice(1, -1);
        let tree = args(str);
        return tree;
      } else throw "Syntax error: Expression must be in parentheses";
    } catch(e) {
      console.error(e);
    }
  },

  eval: function (str) {
    
    function applyFn(node) {
      let fn, value;
      let args = [];

      try {
        /* For a tree fragment, the head node is the function, and the head
         * node's immediate children are the args (i.e., the siblings of its
         * first child). Sequentially put the child args into an array.
         *
         * If one of its children has children of its own, process the tree
         * fragment starting at that child and put the resulting value into
         * the array instead of the child.
         */
        if (node && node.child) {
          fn = node.data;

          for (let child = node.child; child != null; child = child.sibling) {
            let arg = (child.child) ? applyFn(child) : child.data;
            args.push(arg);
          }
        } else throw "Invalid expression";

        if (fn && fn in Scheme) {
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
// let newNums = Scheme.map(ls, x => x + 1);
// console.log(`map +1 ${ls} => ${newNums}`);

let inputs = [
//  [Scheme.cons, ["a", "b"]],
//  [Scheme.car, ls],
//  [Scheme.cdr, ls],
//  [Scheme.reverse, ls],
//  [Scheme.list, ["a", "b", "c"]],
  [Scheme.eval, "(cons 1 2)"],
  [Scheme.eval, "(cons (cons 1 2) 3)"],
  [Scheme.eval, "(list 1 2 3)"],
//  [Scheme.eval, "(cons 1 (cons 2 3))"],
//  [Scheme.eval, "(append (cons 1 2) (list 3 4))"], // should throw error
//  [Scheme.eval, "(list 0 (cons 1 2)"], // should throw error
  [Scheme.eval, "(list 0 (cons 1 2) (cons 3 (cons 4 5)))"] //TODO should not throw error
];

for (let [fn, arg] of inputs) {
  test(fn, arg);
}



