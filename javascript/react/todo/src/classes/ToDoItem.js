import Sugar from "sugar-date";
import Utilities from "./Utilities";

class ToDoItem {
  task;
  deadline;
  deadlineDate;
  isDone;
  id;

  constructor({task, deadline = null, isDone = false}) {
    this.task = task;
    this.deadline = deadline;
    this.deadlineDate = ToDoItem.dateFromString(deadline);
    this.isDone = isDone;
    this.id = crypto.randomUUID();
  }
  
  static dateFromString(deadline) {
    let test, date;
    if (deadline) {
      test = Sugar.Date.create(deadline);
      date = (new Sugar.Date(test).isValid().valueOf()) ? test : null;
    } else {
      date = null;
    }
    return date;
  }

  get deadlineString() {
    if (this.deadlineDate) {
      return `(${Sugar.Date.format(this.deadlineDate, "%c")})`;
    } else if (this.deadline) {
      return `(${this.deadline})`;
    } else {
      return "";
    }
  }

  static doneOrNot(isDone) {
    return (isDone) ? "done" : "notDone";
  }

  static activeOrNot(isDone) {
    return (isDone) ? "inactive" : "active";
  }

  get doneStatus() {
    return ToDoItem.doneOrNot(this.isDone);
  }

  get activeStatus() {
    return ToDoItem.activeOrNot(this.isDone);
  }

  static toggled(item) {
    return new ToDoItem({...item, isDone: !item.isDone});
  }

  Span() {
    return function() {
      let deadlineClassList = "todoDeadline";

      let test = new Sugar.Date(this.deadlineDate);
      if (this.deadlineDate && test.isPast().valueOf()) {
        deadlineClassList += " pastDue";
      }

      return(
        <span className="todoItem">
          {this.task}
          <span className={deadlineClassList}>{this.deadlineString}</span>
        </span>
      );
    }.bind(this);
  }
}

class ToDoList extends Array {
  static toSortedByDate(items) {
    let [deadlines, noDeadlines] = Utilities.partition(items,
      (i => i.deadline !== null));

    let [dates, noDates] = Utilities.partition(deadlines,
      (i => i.deadlineDate !== null));

    let datesSorted = dates.toSorted(
      (a, b) => a.deadlineDate - b.deadlineDate);

    function stringCompareFn(a, b) {
      [a, b] = [a, b].map(i => i.toLowerCase());
      if (a < b) return -1;
      else if (a > b) return 1;
      else return 0;
    }

    let noDatesSorted = noDates.toSorted(
      (a, b) => stringCompareFn(a.deadline, b.deadline));

    let noDeadlinesSorted = noDeadlines.toSorted(
      (a, b) => stringCompareFn(a.task, b.task));

    return [...noDeadlinesSorted, ...noDatesSorted, ...datesSorted];
  }

  static isSorted(items) {
    let sorted = ToDoList.toSortedByDate(items);
    let compared = items.map((item, index) => item === sorted[index]);
    let tested = compared.every(i => i === true);
    return tested;
  }

  static append(items, item) {
    return [...items, item];
  }

  static moveWithinArray(items, fromID, toID) {
    console.log(`Move from item ${fromID} to item ${toID}`);
    function insertBefore(array, matchFn, item) {
      let insertPoint = array.findIndex(matchFn);
      let before = array.slice(0, insertPoint);
      let after = array.slice(insertPoint);
      return [...before, item, ...after];
    }

    let itemToMove = items.find(i => i.id === fromID);
    let rest = items.filter(i => i !== itemToMove);

    let newItems = [];
    if (toID === "bottom") {
      console.log("Move item to bottom");
      newItems = [...rest, itemToMove];
    } else {
      console.log("Insert item");
      newItems = insertBefore(rest, (i => i.id === toID), itemToMove);
    }
    return newItems;
  }

  static toggleDoneStatus(items, item) {
    let toggledItem = ToDoItem.toggled(item);
    console.log(`Marking item as ${toggledItem.doneStatus}`);

    let split = items.indexOf(item);
    let before = items.slice(0, split);
    let after = items.slice(split + 1);
    return [...before, toggledItem, ...after];
  }

  static removeItem(items, item) { 
    return items.filter(i => i !== item);
  }
  
  static setAllItemStatus(items, isDone) {
    return items.map(i => new ToDoItem({...i, isDone: isDone}));
  }

  static areAnyLeftToDo(items) {
    return items.some(i => i.isDone === false);
  }
  
  static areAnyDone(items) {
    return items.some(i => i.isDone === true);
  }
}

export { ToDoItem, ToDoList };
