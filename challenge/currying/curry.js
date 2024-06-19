
function identityA(n) {
  return n;
}

const identityB = (n) => n;

function sum(n, m) {
  return n + m;
}

function partial(fn, arg) {
  return function(rest) {
    return fn(arg, rest);
  }
}

function plusAny(n) {
  return partial(sum, n);
}

function plusTwo(n) {
  return partial(sum, 2)(n);
}

function plusTwoPartialA() {
  return function(n) {
    return plusAny(2)(n);
  }
}

function plusTwoPartialB(n) {
  return plusAny(2)(n);
}

function plusTwoPartialC() {
  return plusAny(2);
}

const plusTwoPartialD = () => plusAny(2);


console.log(identityA(3));
console.log(identityB(3));
console.log(1 + 2);
console.log(plusAny(1)(2));
console.log(plusTwo(1));
console.log(plusTwoPartialA()(1));
console.log(plusTwoPartialB(1));
console.log(plusTwoPartialC()(1));
console.log(plusTwoPartialD()(1));
