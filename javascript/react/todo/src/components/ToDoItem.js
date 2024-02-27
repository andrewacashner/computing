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
  toSortedByDate() {
    let [deadlines, noDeadlines] = Utilities.partition(this,
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

  isSorted() {
    let sorted = this.toSortedByDate();
    let compared = this.map((item, index) => item === sorted[index]);
    let tested = compared.every(i => i === true);
    return tested;
  }
}

export { ToDoItem, ToDoList };
