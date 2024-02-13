// Scheme interpreter in Javascript
// Andrew A. Cashner 2024/02/01

/* TODO (12/9)
 * - Make sure implementation of cons, list, and '() is correct. (RESOLVED)
 *    - In particular, is '() = ListItem { data: null, next: null }, or is '()
 *    = null? (RESOLVED: It's the first. (??))
 *    - Is a cons pair one where the next item is data instead of another
 *    ListItem? (RESOLVED: Yes. (??))
 *    - Is a list a series of cons pairs where the last one has '() as its
 *    next? (and see question about '() above)
 * - We won't process starting expression unless enclosed in parens, but Guile
 *   will just (evaluate and) return the argument; if more than one, Guile
 *   returns the last. (RESOLVED, though we're also not evaluating any but the
 *   last)
 * - Deal with quoted arguments and expressions. ('a -> a, '(1 2 3) -> (list 1
 *   2 3), etc.)
 *    - Better handling of '() everywhere
 * - Deal with trailing unbalanced parens: (cons 1 2)) should return error
 */

import Node from "./tree.mjs";
import { spawn } from "node:child_process";

const DEBUG = false;  /* Display diagnostic messages with debug() */

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
      let str = "";
      if (!isNull(node)) {
        str = "";
        str = node.data;

        if (node.next && !isNull(node.next)) {
          if (node.next instanceof ListItem) {
            str += " " + inner(node.next);
          } else {
            str += " . " + node.next;
          } 
        } 
        return str;
      }
    }

    if (isNull(this)) {
      return "()";
    } else {
      let sexpr = inner(this);
      return `(${sexpr})`;
    }
  }
}

class Ratio {
  constructor(numerator, denominator) {
    let num = Number(numerator);
    let denom = Number(denominator);
    if (Number.isInteger(num) && Number.isInteger(denom)) {
      this.numerator = numerator;
      this.denominator = denominator;
    } else throw "Invalid ratio";
  }

  static parse(str) {
    let value;
    let test = str.match(/^(?<numerator>[0-9]*)\/(?<denominator>[0-9]*)$/);
    if (test) {
      value = new Ratio(test.groups.numerator, test.groups.denominator);
    }
    return value;
  }

  toFloat() {
    return this.numerator / this.denominator;
  }

  valueOf() {
    return this.toFloat();
  }

  toString() {
    return `${this.numerator}/${this.denominator}`;
  }
}

function isNull(obj) {
  return obj instanceof ListItem 
    && !obj.data 
    && !obj.next;
}

function isPair(obj) {
  debug(obj instanceof ListItem);
  debug(obj.next);
  return obj instanceof ListItem 
    && obj.next != null;
}

function isList(obj) {
  let head = obj;
  while (head.next) {
    head = head.next;
  }
  return isNull(head);
}

function scmNumber(str) {
  let value = Number(str);
  if (isNaN(value)) {
    value = Ratio.parse(str);
  }
  return value;
};

function parse(str) {
  const addToTree = (tree, node) => (!tree) ? node : tree.appendChild(node);

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
            let node = parse(addCharToArg); 
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
    } else {
      let atoms = str.split(" ");
      let tree = new Node(atoms.at(-1));
      debug(tree);
      return tree;
    }
    //  throw "Syntax error: Expression must be in parentheses";
  } catch(e) {
    console.error(e);
  }
}

function read(str) {

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
    if (node) {
      fn = node.data;

      if (node.child) {
        for (let child = node.child; child != null; child = child.sibling) {
          let arg;
          if (child.child) {
            arg = applyFn(child);
          } else if (child.data === "_nil") {
            arg = Scheme["'()"];
          } else arg = child.data;

          if (arg) {
            args.push(arg);
          }
        }
      } 
    }

    if (fn) {
      if (fn in Scheme) {
        debug(`Apply fn ${fn} to args ${args}`);
        value = Scheme[fn](...args);
      } else if (fn.startsWith("'")) {
        value = fn.slice(1);
      } else if (fn === "_nil") {
        value = Scheme["'()"];
      } else if (fn === "#t" || fn === "#f") {
        value = scmBool(fn);
      } else if (scmNumber(fn)) {
        value = scmNumber(fn);
        debug(value.toString());
      } else throw `Unbound variable: #{${fn}}#`;
    } else throw "Nothing to process";
    return value;
    // TODO need to process arguments the same way, to filter '(), #t, #f
  }

  try {
    // Replace '() with nil, otherwise our paren parsing doesn't work
    // TODO deal with quoted expressions generally
    let cleaned = str.replace(/'\(\)/u, "_nil");
      let tree = parse(cleaned);
      if (tree) {
        debug(`${tree}`);
        return applyFn(tree);
      } else throw "Could not evaluate expression";
  } catch(e) {
    console.error(e);
  }
}

function maybeBool(s) {
  return (s === true || s === false) ? Scheme[s] : s; 
}

function scmBool(s) {
  if (s === "#t") {
    return true;
  } else if (s === "#f") {
    return false;
  } else return s;
}

const Scheme = { 
  true: "#t",
  
  false: "#f",

  "'()": new ListItem(),

  cons: (a, b) => new ListItem(a, b),

  car: item => item.data,

  cdr: item => item.next,

  list: function(...items) {
    let cell = null;
    let next = Scheme["'()"];
    for (let i = items.length - 1; i >= 0; --i) {
      cell = Scheme.cons(items[i], next);
      next = cell;
    }
    return cell;
  },

  last: ls => (isNull(ls.next)) ? ls.data : Scheme.last(ls.next),

  "null?": obj => Scheme[isNull(obj)],
 
  "pair?": obj => Scheme[isPair(obj)],
 
  "list?": obj => Scheme[isList(obj)],

  reverse: function (ls, tmp = null) {
    if (!ls) {
      return tmp;
    } else if (isNull(ls)) {
      return tmp ?? Scheme["'()"];
    } else {
      return Scheme.reverse(Scheme.cdr(ls), Scheme.cons(Scheme.car(ls), tmp));
    }
  },

  // TODO this only handles single-argument functions?
  map: function (ls, fn, tmp = null) {
    debug(`map fn ${fn} to ls ${ls}`);
    if (isNull(ls)) {
      return Scheme.reverse(tmp);
    } else {
      return Scheme.map(Scheme.cdr(ls), fn, Scheme.cons(fn(Scheme.car(ls)), tmp));
    }
  },

  // TODO account for Scheme-specific concept of truthiness/falsiness
  // Handle IO better with #t and #f
  if: (test, ifTrue, ifFalse) => scmBool(test) ? ifTrue : ifFalse,

  or: (a, b) => maybeBool(scmBool(a) || scmBool(b)),

  and: (a, b) => maybeBool(scmBool(a) && scmBool(b)),
}

async function compareGuile(input, jsOutput) {
  const guile = spawn("guile", ["--use-srfi=1", "-c", `(display ${input})`]);
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

export { Scheme, read, compareGuile };
