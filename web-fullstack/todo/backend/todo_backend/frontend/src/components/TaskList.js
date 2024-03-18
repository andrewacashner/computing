import { useContext } from "react";
import ToDoContext from "../store/ToDoContext";
import ListItem from "./ListItem";

// Drag and drop functions
function isDraggedOverSelf(event) {
  return event.target.id === event.dataTransfer.getData("text/uuid");
}

function isDraggedOverNext(event, items) {
  let dragged = event.dataTransfer.getData("text/uuid");
  let current = event.target.id;
  const getIndex = id => items.list.findIndex(i => i.id === id);
  return getIndex(current) - getIndex(dragged) === 1;
}

function TaskList() {
  let context = useContext(ToDoContext);
  let items = context.get;
  let setItems = context.set;

  function dropListItem(event) {
    event.preventDefault()

    if (items.list.length > 1) {
      let fromID = event.dataTransfer.getData("text/uuid");

      // Check if item was dropped below the list items, in the extra space we
      // leave at bottom of the ol.todo
      let toID = (event.target.className === "todo") 
        ? "bottom" : event.target.id;

      if (fromID !== toID && !isDraggedOverNext(event, items)) {
        setItems(prevItems => prevItems.moveWithinArray(fromID, toID));
      }
    }
  }

  function dragoverListItem(event) {
    event.preventDefault();
    if (event.target.tagName === "LI" 
      && !isDraggedOverSelf(event) 
      && !isDraggedOverNext(event, items)) {
      event.target.classList.add("gapAbove");
    }
  }
 
  function makeListItem(item) {
    return(
      <ListItem key={item.id}>{item}</ListItem>
    );
  }

  if (items.list.length > 0) {
    let [done, notDone] = items.partition(i => i.isDone);
    
    const ItemsDone = () => (done) ? done.list.map(makeListItem) : null;
    const ItemsNotDone = () => (notDone) ? notDone.list.map(makeListItem) : null;

    return(
      <section id="lists">
        <ol className="todo"
            onDragOver={dragoverListItem}
            onDrop={dropListItem}>
          <ItemsNotDone />
        </ol>
        <ol className="todoDone">
          <ItemsDone />
        </ol>
      </section>
    );
  }
}

export default TaskList;

