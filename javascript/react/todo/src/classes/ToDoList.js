import ToDoItem from "./ToDoItem";

export default class ToDoList {
  list;
  draftEntry;

  constructor(list = [], draft = new ToDoItem()) {
    this.list = [...list];
    this.draftEntry = draft;
  }

  cloneWithDraft(draft) {
    return new ToDoList(this.list, draft);
  }

  moveItemToDraft(item) {
    return this.removeItem(item).cloneWithDraft(item);
  }

  partition(testFn) {
    let results = { true: [], false: [] };
    if (this.list.length > 0) {
      this.list.forEach(i => results[testFn(i)].push(i));
    }
    return [results.true, results.false].map(r => new ToDoList(r));
  }

  toSortedByDate() {
    let [deadlines, noDeadlines] = this.partition(i => i.deadline !== null);

    let [dates, noDates] = deadlines.partition(i => i.deadlineDate !== null);

    let datesSorted = dates.list.toSorted(
      (a, b) => a.deadlineDate - b.deadlineDate);

    function stringCompareFn(a, b) {
      [a, b] = [a, b].map(i => i.toLowerCase());
      if (a < b) return -1;
      else if (a > b) return 1;
      else return 0;
    }

    let noDatesSorted = noDates.list.toSorted(
      (a, b) => stringCompareFn(a.deadline, b.deadline));

    let noDeadlinesSorted = noDeadlines.list.toSorted(
      (a, b) => stringCompareFn(a.task, b.task));

    let newItems = [...noDeadlinesSorted, ...noDatesSorted, ...datesSorted];
    return new ToDoList(newItems);
  }

  hasIdenticalContents(other) {
    let compared = this.list.map((item, index) => item === other.list[index]);
    let tested = compared.every(i => i === true);
    return tested;
  }
 
  isSorted() {
    let sorted = this.toSortedByDate();
    return this.hasIdenticalContents(sorted);
  }

  append(item) {
    return new ToDoList([...this.list, item]);
  }

  moveWithinArray(fromID, toID) {
    console.log(`Move from item ${fromID} to item ${toID}`);
    function insertBefore(array, matchFn, item) {
      let insertPoint = array.findIndex(matchFn);
      let before = array.slice(0, insertPoint);
      let after = array.slice(insertPoint);
      return [...before, item, ...after];
    }

    let itemToMove = this.list.find(i => i.id === fromID);
    let rest = this.list.filter(i => i !== itemToMove);

    let newItems = [];
    if (toID === "bottom") {
      console.log("Move item to bottom");
      newItems = [...rest, itemToMove];
    } else {
      console.log("Insert item");
      newItems = insertBefore(rest, (i => i.id === toID), itemToMove);
    }
    return new ToDoList(newItems);
  }

  removeItem(item) { 
    let filtered = this.list.filter(i => i !== item);
    return new ToDoList(filtered);
  }

  toggleDoneStatus(item) {
    let toggledItem = item.toggled();
    console.log(`Marking item as ${toggledItem.doneStatus}`);

    let items = this.list;
    let split = items.indexOf(item);
    let before = items.slice(0, split);
    let after = items.slice(split + 1);
    return new ToDoList([...before, toggledItem, ...after]);
  }

  setAllItemStatus(isDone) {
    let adjusted = this.list.map(i => new ToDoItem({...i, isDone: isDone}));
    return new ToDoList(adjusted);
  }

  areAnyLeftToDo() {
    return this.list.some(i => i.isDone === false);
  }
  
  areAnyDone() {
    return this.list.some(i => i.isDone === true);
  }

  activeOrNot(test) {
    return (test) ? "active" : "inactive";
  }

  checkAllStatus() {
    return this.activeOrNot(this.areAnyLeftToDo());
  }

  uncheckAllStatus() {
    return this.activeOrNot(this.areAnyDone());
  }

  unsortedStatus() {
    return this.activeOrNot(!this.isSorted());
  }
}
