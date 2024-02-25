import Sugar from "sugar-date";

export default class ToDoItem {
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
    }
    return date;
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

  get deadlineString() {
    let deadline = "";
    if (this.deadlineDate) {
      let dateString = Sugar.Date.format(this.deadlineDate, "%c");
      deadline = `(${dateString})`;
    } else if (this.deadline) {
      deadline = `(${this.deadline})`;
    }
    return deadline;
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
