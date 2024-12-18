function sumDigits() {
  digits = Array.from({length: 10}, (_, i) => i);
  return 1 + digits.reduce((acc, n) => acc + 5 * (9 + 2 * n), 0);
}

console.log(sumDigits());
