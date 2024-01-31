"use strict";

const KEY = "Much that is hidden shall be revealed";

class CaesarCipher {
  shift;
  plaintext;
  encoded;

  constructor(shift, msg) {
    this.shift = shift;
    this.plaintext = msg;
    this.encoded = this.#encodeStr();
  }

  static lowercase = "abcdefghijklmnopqrstuvwxyz";
  static uppercase = this.lowercase.toUpperCase();

  #base(alphabet) {
    return function (c) {
      let newC = null;
      if (alphabet.includes(c)) {
        let index = alphabet.indexOf(c) + this.shift;
        if (index < 0 || index > alphabet.length - 1) {
          index -= Math.sign(index) * alphabet.length
        }
        newC = alphabet.at(index);
      } 
      return newC;
    }.bind(this);
  }

  #lower = this.#base(CaesarCipher.lowercase);
  #upper = this.#base(CaesarCipher.uppercase);

  #encodeChar = (c) => this.#lower(c) ?? this.#upper(c) ?? c;

  #encodeStr = () => this.plaintext.split("").map(this.#encodeChar).join("");
}

const random = {
  n: (max) => Math.floor(Math.random() * max),
  wordIndex: (text) => random.n(text.split(" ").length),
  word: (text) => {
    let words = text.split(" ");
    return words.at(random.n(words.length));
  }
}

function encode(book, msg) {
  
  function bookKey(shift, text) {
    let key = random.word(text);
    let encodedKey = new CaesarCipher(shift, key).encoded;
    let position = text.split(" ").indexOf(key);
    return `${position} ${encodedKey}`;
  }

  let shift = random.wordIndex(book);
  let encodedKey = bookKey(shift, book);
  let encodedMsg = (new CaesarCipher(shift, msg)).encoded;
  return `${encodedKey} ${encodedMsg}`;
}

function decode(book, msg) {
  
  function letterDiff(c1, c2) {
    function letterValue(c) {
      return CaesarCipher.lowercase.indexOf(c.toLowerCase());
    }
    return letterValue(c1) - letterValue(c2);
  }

  let words = msg.split(" ");
  let index = Number(words[0]);
  let encodedKey = words[1];

  let sourceKey = book.split(" ").at(index);
  let shift = letterDiff(encodedKey[0], sourceKey[0]);

  let body = words.slice(2).join(" ");
  return new CaesarCipher(-shift, body).encoded;
}

document.addEventListener("DOMContentLoaded", () => {
  let msg = prompt("Enter the message to encode");
  let answer = encode(KEY, msg);
  let encoded = document.querySelector("p.encode");
  encoded.textContent = answer;

  let button = document.querySelector("button#decode");
  button.addEventListener("click", () => {
    let decoded = document.querySelector("p.decode");
    decoded.textContent = decode(KEY, answer);
  });
});


