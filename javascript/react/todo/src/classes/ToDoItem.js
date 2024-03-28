import Sugar from "sugar-date";

export default class ToDoItem {
  id;
  task;
  deadline;
  deadlineDate;
  isDone;
  userOrder;

  constructor({
    id = crypto.randomUUID(), 
    task = "", 
    deadline = null, 
    isDone = false,
    userOrder = 0
  } = {}) {
    this.id = id;
    this.task = task;
    this.deadline = deadline;
    this.deadlineDate = ToDoItem.dateFromString(deadline);
    this.isDone = isDone;
    this.userOrder = userOrder;
  }

  clone() {
    return new ToDoItem(this);
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

  get doneStatus() {
    return (this.isDone) ? "done" : "notDone";
  }

  get activeStatus() {
    return (this.isDone) ? "inactive" : "active";
  }

  toggled() {
    return new ToDoItem({...this, isDone: !this.isDone});
  }

}

