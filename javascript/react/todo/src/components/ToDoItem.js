export default class ToDoItem {
  task;
  deadline;
  isDone;

  constructor({task, deadline = null, isDone = false}) {
    this.task = task;
    this.deadline = deadline;
    this.isDone = isDone;
    this.id = crypto.randomUUID();
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

  toString() {
    let deadline = (this.deadline) ? ` (${this.deadline})` : "";
    return this.task + deadline;
  }
}
