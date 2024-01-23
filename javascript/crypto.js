#!/usr/bin/env node

// Cryptography with Node.js
// Andrew Cashner
// 2024/04/23

const LOWERCASE = "abcdefghijklmnopqrstuvwxyz";
const UPPERCASE = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
const ALPHABET = LOWERCASE + UPPERCASE;

function circularShift(start, n, source) {
  let newIndex = start + n;
  let max = source.length;

  if (newIndex < 0) {
    newIndex += max;
  } else if (newIndex >= max) {
    newIndex -= max;
  }
  return newIndex;
}

function shiftedChar(oldStr, index, shift, source) {
  let newIndex = circularShift(index, shift, source);
  let newChar = source.charAt(newIndex);
  return newChar;
}

function isUpperCase(c) {
  return c === c.toUpperCase();
}

function shift(str, n) {
  let newStr = "";
  for (let c of str) {
    let index = ALPHABET.indexOf(c);

    if (index >= 0) {
      let source = LOWERCASE;
      if (isUpperCase(c)) {
        source = UPPERCASE;
        index = UPPERCASE.indexOf(c);
      };

      newStr += shiftedChar(str, index, n, source);
    } else {
      newStr += c;
    }
  }
  return newStr;
}

async function main() {
  let data = "";
  for await (let chunk of process.stdin) {
    data += chunk;
  }

  if (process.argv.length < 3) {
    console.error("Need to supply a shift amount");
  } else {
    shift_amount = Number(process.argv[2]);
    let code = shift(data, shift_amount);
    process.stdout.write(code);
  }
}

main();


