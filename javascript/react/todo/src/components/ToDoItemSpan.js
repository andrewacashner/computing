import Sugar from "sugar-date";

export default function ToDoItemSpan(props) {
  let item = props.children;
  let deadlineClassList = "todoDeadline";

  let test = new Sugar.Date(item.deadlineDate);
  if (item.deadlineDate && test.isPast().valueOf()) {
    deadlineClassList += " pastDue";
  }

  return(
    <span className="todoItem">
      {item.task}
      <span className={deadlineClassList}>{item.deadlineString}</span>
    </span>
  );
}
