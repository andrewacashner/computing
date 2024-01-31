const KEY = "Much that is hidden shall be revealed"

const LOWERCASE = "abcdefghijklmnopqrstuvwxyz";
const UPPERCASE = LOWERCASE.toUpperCase();

const caesar = {
  base: (alphabet, shift, c) => {
    let newC = null;
    if (alphabet.includes(c)) {
      let index = alphabet.indexOf(c);
      let newIndex = index + shift;
      if (newIndex < 0 || newIndex > alphabet.length - 1) {
        newIndex -= Math.sign(newIndex) * alphabet.length
      }
      newC = alphabet.at(newIndex);
    } 
    return newC;
  },
  lower: (shift, c) => caesar.base(LOWERCASE, shift, c),
  upper: (shift, c) => caesar.base(UPPERCASE, shift, c),
  encode: (shift, c) => caesar.lower(shift, c) ?? caesar.upper(shift, c) ?? c
}

function toCaesar(shift, s) {
  let chars = s.split("");
  let encodedChars = chars.map(c => caesar.encode(shift, c));
  let encodedStr = encodedChars.join("");
  return encodedStr;
}

function rand(max) {
  return Math.floor(Math.random() * max);
}
function randWord(text) {
  let words = text.split(" ");
  return words.at(rand(words.length));
}

function bookKey(shift, text) {
  let key = randWord(text);
  let encodedKey = toCaesar(shift, key);
  let words = text.split(" ");
  let position = words.indexOf(key);
  return `${position} ${encodedKey}`;
}

function bookShift(text) {
  return rand(text.split(" ").length);
}

function encode(book, msg) {
  let shift = bookShift(book);
  console.log(shift);
  let encodedKey = bookKey(shift, book);
  let encodedMsg = toCaesar(shift, msg);
  return `${encodedKey} ${encodedMsg}`;
}

function letterDiff(c1, c2) {
  function letterValue(c) {
    return LOWERCASE.indexOf(c.toLowerCase());
  }
  return letterValue(c1) - letterValue(c2);
}

function decode(book, msg) {
  let words = msg.split(" ");
  let index = Number(words[0]);
  let encodedKey = words[1];
  let sourceKey = book.split(" ").at(index);
  let shift = letterDiff(encodedKey[0], sourceKey[0]);
  let body = words.slice(2).join(" ");
  return toCaesar(-shift, body);
}

document.addEventListener("DOMContentLoaded", () => {
  msg = prompt("Enter the message to encode");
  answer = encode(KEY, msg);
  let encoded = document.querySelector("p.encode");
  encoded.textContent = answer;

  let button = document.querySelector("button#decode");
  button.addEventListener("click", () => {
    let decoded = document.querySelector("p.decode");
    decoded.textContent = decode(KEY, answer);
  });
});
