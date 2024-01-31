#!/usr/bin/env node
// Return the mean of a given list of numbers (comma-separated)
// Andrew Cashner, 2024/01/31

function mean(nums) {
  let sum = nums.reduce((a, b) => a + b, 0);
  return sum / nums.length;
}

function main() {
  let input = process.argv.slice(2);
  let nums = input.map(Number);
  if (nums.some(n => isNaN(n))) {
    console.log("Usage: average n1 n2 [n3 n4...] (All arguments must be numbers)");
  } else {
    let avg = mean(nums);
    console.log(avg);
  } 
}

main();

