#!/usr/bin/env node

class Task {
  desc;
  isDone;

  constructor(desc = "", isDone = false) {
    this.desc = desc;
    this.isDone = isDone;
  }
}

function partition(array, test) {
  let results = { true: [], false: [] };
  array.forEach(task => results[test(task)].push(task));
  return [results.true, results.false];
}

let items = [["Go", true], ["Leave", false], ["Come", false], ["Stay", true]];
let tasks = items.map(i => new Task(...i));
console.log(tasks);

let [done, notDone] = partition(tasks, (t => t.isDone));
console.log(done);
console.log(notDone);


