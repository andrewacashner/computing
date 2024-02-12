// Scheme interpreter in Javascript
// Andrew A. Cashner 2024/02/01

/* TODO (12/9)
 * - Make sure implementation of cons, list, and '() is correct.
 *    - In particular, is '() = ListItem { data: null, next: null }, or is '()
 *    = null?
 *    - Is a cons pair one where the next item is data instead of another
 *    ListItem?
 *    - Is a list a series of cons pairs where the last one has '() as its
 *    next? (and see question about '() above)
 * - We won't process starting expression unless enclosed in parens, but Guile
 *   will just (evaluate and) return the argument; if more than one, Guile
 *   returns the last.
 * - Deal with quoted arguments and expressions. ('a -> a, '(1 2 3) -> (list 1
 *   2 3), etc.)
 * - Deal with trailing unbalanced parens: (cons 1 2)) should return error
 */

import Node from "./tree.mjs";
import { spawn } from "node:child_process";

const DEBUG = true;  /* Display diagnostic messages with debug() */

function debug(msg) {
  if (DEBUG) {
    console.debug(msg);
  }
}

class ListItem {
  data;
  next;

  constructor(data = null, next = null) {
    this.data = data;
    this.next = next;
  }

  toString() {
    
    function inner(node) {
      if (!node.data && !node.next) {
        return "()";
      } else {
        let str = node.data;

        if (node.next) {
          if (node.next instanceof ListItem) {
            str += " " + inner(node.next);
          } else {
            str += " . " + node.next;
          } 
        } 
        return str;
      }
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

  _isPair: (obj) => obj instanceof ListItem && obj.next,

  _isList: (obj) => Scheme._last(obj) instanceof ListItem,

  "pair?": (obj) => Scheme._bool(Scheme._isPair(obj)),

  "list?": (obj) => Scheme._bool(Scheme._isList(obj)),

  _nil: null, 

  "'()": () => Scheme._nil,

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
      OUTER:      Symbol("outer"),
      WORD:       Symbol("word"),
      EXPR:       Symbol("expr"),
      QUOTATION:  Symbol("quote"),
      QUOTEDEXPR: Symbol("quotedExpr")
    }

    const isWhiteSpace = c => /\s/.test(c);
    const OPEN  = "(";
    const CLOSE = ")";
    const QUOTE = "'";

    // Split a string into arguments;
    // Arguments can be space-delimited words OR s-expressions delimited with
    // parentheses;
    // Return an array of strings containing either the words or the
    // parenthetical expressions; do not parse within the parentheses.
    function args(str, thisArg = "", tree = null, mode = SyntaxMode.OUTER, exprLevel = 0) {

      if (!str) {
        if (exprLevel !== 0) {
          // We got to the end of the input string but the parentheses were
          // not balanced.
          throw `Syntax error: Unbalanced parentheses (level ${exprLevel})`;
        }
        if (thisArg) {
          // We got to the end and we still have an argument we found that we
          // haven't added yet
          let node = new Node(thisArg);
          tree = addToTree(tree, node);
        }
        return tree;
      } else {
        let c = str[0];
       
        // Shorthands for handling input, short-term memory, and output for
        // use in the different parsing situations
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

        const skipAndContinueInMode = (newMode) => args(advanceStr, 
          thisArg, tree, newMode, exprLevel);
        
        if (isWhiteSpace(c)) {
          if (mode === SyntaxMode.EXPR || SyntaxMode.QUOTEDEXPR) {
            // Copy whitespace in EXPR mode
            return copyAndContinueInMode(SyntaxMode.EXPR);

          } else if (mode === SyntaxMode.WORD || SyntaxMode.QUOTATION) {
            // First whitespace in WORD or QUOTATION mode signals end of the
            // argument; copy the argument and continue processing in OUTER
            // mode
            let node = new Node(thisArg);
            return args(advanceStr, resetWord, addNode(node), 
              SyntaxMode.OUTER, exprLevel);

          } else return skipAndContinue(); // Otherwise skip whitespace

        } else if (c === OPEN) {
          // Open paren signals start of sexpr; increment level to track balanced parens
          if (mode === SyntaxMode.QUOTATION) {
            return args(advanceStr, addCharToArg, tree, 
              SyntaxMode.QUOTEDEXPR, ++exprLevel);
          } else {
            return args(advanceStr, addCharToArg, tree, 
              SyntaxMode.EXPR, ++exprLevel);
          }

        } else if (c === CLOSE 
                  && (mode === SyntaxMode.EXPR || SyntaxMode.QUOTEDEXPR)) {
          // Close paren signals end of sexpr when in expression mode;
          // decrement level to track balanced parens; keep copying unless we
          // are back at start level; in that case the current argument is a
          // complete expression. If in "quoted expression" mode, save the
          // whole expression.  If in normal expression mode, and the string
          // within parentheses contains a parenthetical expression, recurse
          // to parse the expression and then add the result to the tree.
          if (exprLevel > 1) {
            return args(advanceStr, addCharToArg, tree, 
              SyntaxMode.EXPR, --exprLevel);

          } else {
            thisArg = addCharToArg;
            let node;
            let inner = thisArg.slice(1, -1);
            if ((inner.search("/\s\([^\)]*\)/") >= 0)
                && mode !== SyntaxMode.QUOTEDEXPR) {
                debug(`Parse inner expression: ${inner}`);
                node = Scheme._parse(inner);
            } else {
              debug("Capture whole expression");
              node = new Node(thisArg);
              debug(node);
            }
            debug("Process expression");
            return args(advanceStr, resetWord, addNode(node),
              SyntaxMode.OUTER, --exprLevel);
          } 

        } else if (c === QUOTE && mode === SyntaxMode.OUTER) {
          // First quote character signals quotation mode (don't copy the
          // quote character)
          return skipAndContinueInMode(SyntaxMode.QUOTATION);

        } else {
          // First normal char (not whitespace, paren or quote) in outer mode
          // signals the start of a word. Once in word mode, just keep
          // copying.
          if (mode === SyntaxMode.OUTER) {
            return copyAndContinueInMode(SyntaxMode.WORD);

          } else return copyAndContinue();
        }
      }
    }
    try {
      debug(`Parse str '${str}'`);
      //if (str.startsWith("(") && str.endsWith(")")) {
      //  str = str.slice(1, -1);
        return args(str);
      //} else throw "Syntax error: Expression must be in parentheses";
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

async function compareGuile(input, jsOutput) {
  const guile = spawn("guile", ["-c", `(display ${input})`]);
  let msg;
  for await (let chunk of guile.stdout) {
    let guileOutput = chunk.toString();
    let guileStatus = (guileOutput === jsOutput) ? "OK" : guileOutput;
    console.debug(`[Guile: ${guileStatus}]`);
  }
  for await (let chunk of guile.stderr) {
    console.debug(`[Guile error: ${chunk.toString()}]`);
  }
}

export { Scheme, compareGuile };

