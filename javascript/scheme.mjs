#!/usr/bin/env node
// Scheme interpreter in Javascript
// Andrew A. Cashner 2024/02/01

import Node from "./tree.mjs";

const DEBUG = false;

function debug(msg) {
  if (DEBUG) {
    console.debug(msg);
  }
}

class ListItem {
  data;
  next;

  constructor(data, next = null) {
    this.data = data;
    this.next = next;
  }

  toString() {
    
    function inner(node) {
      let str = node.data;

      if (node.next) {
        if (Scheme._isList(node)) {
          str += " " + inner(node.next);
        } else {
          str += " . " + node.next;
        } 
      } 
      return str;
    }

    let sexpr = inner(this);
    return `(${sexpr})`;
  }
}

const Scheme = {

  cons: (a, b) => new ListItem(a, b),

  car: item => item.data,

  cdr: item => item.next,

  list: function(...items) {
    let cell = null;
    let next = null;
    for (let i = items.length - 1; i >= 0; --i) {
      cell = Scheme.cons(items[i], next);
      next = cell;
    }
    return cell;
  },

  _last: function(ls) {
    let head = ls;
    if (!head.next) {
      return head;
    } else {
      return Scheme._last(head.next);
    }
  },

  _true: "#t",

  _false: "#f",

  _bool: (test) => (test) ? Scheme._true : Scheme._false,

  _isPair: (obj) => !(obj.next instanceof ListItem),

  _isList: (obj) => Scheme._last(obj) instanceof ListItem,

  "pair?": (obj) => Scheme._bool(Scheme._isPair(obj)),

  "list?": (obj) => Scheme._bool(Scheme._isList(obj)),

  nil: null, 

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

  _parse: function(str) {
    function addToTree(tree, node) {
      if (!tree) {
        tree = node;
      } else {
        tree.appendChild(node);
      }
      return tree;
    }

    const SyntaxMode = {
      WORD: Symbol("word"),
      EXPR: Symbol("expr"),
      OUTER: Symbol("outer") 
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
        if (exprLevel !== 0) {
          throw `Syntax error: Unbalanced parentheses (level ${exprLevel})`;
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

        const copyAndContinue = () => args(advanceStr, 
          addCharToArg, tree, mode, exprLevel);

        const skipAndContinue = () => args(advanceStr, 
          thisArg, tree, mode, exprLevel);

        const copyAndContinueInMode = (newMode) => args(advanceStr, 
          addCharToArg, tree, newMode, exprLevel);

        
        if (isWhiteSpace(c)) {
          if (mode === SyntaxMode.EXPR) {
            return copyAndContinueInMode(SyntaxMode.EXPR);

          } else if (mode === SyntaxMode.WORD) {
            let node = new Node(thisArg);
            return args(advanceStr, resetWord, addNode(node), 
              SyntaxMode.OUTER, exprLevel);

          } else return skipAndContinue();

        } else if (c === OPEN) {
            return args(advanceStr, addCharToArg, tree, 
              SyntaxMode.EXPR, ++exprLevel);

        } else if (c === CLOSE && mode === SyntaxMode.EXPR) {
            if (exprLevel > 1) {
              return args(advanceStr, addCharToArg, tree, 
                SyntaxMode.EXPR, --exprLevel);

            } else {
              let node = Scheme._parse(addCharToArg); 
              return args(advanceStr, resetWord, addNode(node), 
                SyntaxMode.OUTER, --exprLevel);
            }

        } else {
          if (mode === SyntaxMode.OUTER) {
            return copyAndContinueInMode(SyntaxMode.WORD);

          } else return copyAndContinue();
        }
      }
    }
    try {
      debug(`Parse str '${str}'`);
      if (str.startsWith("(") && str.endsWith(")")) {
        str = str.slice(1, -1);
        return args(str);
      } else throw "Syntax error: Expression must be in parentheses";
    } catch(e) {
      console.error(e);
    }
  },

  eval: function (str) {
    
    function applyFn(node) {
      let fn, value;
      let args = [];

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
          if (child.data === "nil") {
            arg = Scheme.nil;
          }
          args.push(arg);
        }
      } 

      if (fn && fn in Scheme) {
        value = Scheme[fn](...args);
      } else throw `Unknown function '${fn}'`;
      return value;
    }

    try {
      // Replace '() with nil, otherwise our paren parsing doesn't work
      // TODO deal with quoted expressions generally
      let cleaned = str.replace(/'\(\)/u, "nil");
      let tree = Scheme._parse(cleaned);
      if (tree) {
        debug(`${tree}`);
        return applyFn(tree);
      } else throw "Could not evaluate expression";
    } catch(e) {
      console.error(e);
    }
  }
}


let ls = Scheme.list(1, 2, 3);
// let newNums = Scheme.map(ls, x => x + 1);
// console.log(`map +1 ${ls} => ${newNums}`);

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
  [Scheme.eval, "(append (cons 1 2) (list 3 4))"], 
    // should throw error: no function 'append'
  [Scheme.eval, "cons 1 2"], 
    // should throw error: no parens
  [Scheme.eval, "(cons 1 2"], // should throw error: no parens
  [Scheme.eval, "cons 1 (2)"], // should throw error: no parens
  [Scheme.eval, "(list 0 (cons 1 2)"], 
    // should throw error: unbalanced parens
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
  // TODO Guile output is (1 2 3 . 4)
  [Scheme.eval, "(list 0 (cons 1 2) (cons 3 (cons 4 5)))"],
    // TODO Guile output is (0 (1 . 2) (3 4 . 5))
];

function test(fn, expr) {
  let result;
  if (expr instanceof Array) {
    result = fn.apply(null, expr);
  } else {
    result = fn.call(null, expr);
  }
  console.log(`${fn.name} ${expr} => ${result}`);
  return result;
}

for (let [fn, arg] of inputs) {
  test(fn, arg);
}



