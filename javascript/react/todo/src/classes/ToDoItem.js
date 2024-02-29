import Sugar from "sugar-date";

export default class ToDoItem {
  task;
  deadline;
  deadlineDate;
  isDone;
  id;

  constructor({task = "", deadline = null, isDone = false} = {}) {
    this.task = task;
    this.deadline = deadline;
    this.deadlineDate = ToDoItem.dateFromString(deadline);
    this.isDone = isDone;
    this.id = crypto.randomUUID();
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

