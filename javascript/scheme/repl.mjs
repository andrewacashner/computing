#!/usr/bin/env node
// Scheme REPL
// Andrew Cashner, 2024/02/12

import * as scm from "./scheme.mjs";
import * as readline from "node:readline/promises";

const GUILE = true;   /* Show comparison with Guile output */

async function evalLine(io, line, count) {
  let inputPrompt = io.getPrompt();
  let output = "";
  let value = scm.read(line);
  try {
    if (value) {
      output = value.toString();
      io.setPrompt(`\$${++count} = `);
      io.prompt();
      console.log(output);
    } 
  } catch(e) {
    console.error(e);
  } finally {
    if (GUILE) {
      await scm.compareGuile(line, output);
    }
  }
  io.setPrompt(inputPrompt);
  io.prompt();
}

async function main() {
  let count = 0;

  const rl = readline.createInterface({ 
    input: process.stdin, 
    output: process.stdout,
    prompt: "scheme> "
  });

  rl.prompt();
  rl.on("line", (line) => {
    if (line === ",q") {
      rl.close();
    } else evalLine(rl, line, count);
  });
}

main();
