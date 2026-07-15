// DIY tree utility in Node.js
// Andrew Cashner
// 2026/07/15

import * as fs from "node:fs";
import * as path from "node:path";

function showfile(filename, indent) {
  if (indent == 0) {
    console.log(filename);
  } else {
    let preface = "  ".repeat(indent - 1);
    console.log(`${preface}|__ ${filename}`);
  }

}

function display_dir(filepath, indent = 0) {
  showfile(path.basename(filepath), indent);

  let pathStatus = fs.statSync(filepath, { throwIfNoEntry: false });

  if (pathStatus && pathStatus.isDirectory()) {
      let files = fs.readdirSync(filepath, { withFileTypes: true });
      if (files) {
        files.forEach(file => display_dir(`${filepath}/${file.name}`, indent + 1));
      }
  }
}

function main() {
  if (process.argv.length != 3) {
    console.log("Usage: node tree.js PATH");
  } else {
    let path = process.argv[2];
    display_dir(path);
  }
}

main();


