import Utilities from "../classes/Utilities";
import ListItem from "./ListItem";

// Drag and drop functions
function moveWithinArray(items, fromID, toID) {
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

function isDraggedOverSelf(event) {
  return event.target.id === event.dataTransfer.getData("text/uuid");
}

function isDraggedOverNext(event, items) {
  let dragged = event.dataTransfer.getData("text/uuid");
  let current = event.target.id;
  const getIndex = id => items.findIndex(i => i.id === id);
  return getIndex(current) - getIndex(dragged) === 1;
}

function TaskList(props) {
  let [items, setItems] = props.items;

  function dropListItem(event) {
    event.preventDefault()

    if (items.length > 1) {
      let fromID = event.dataTransfer.getData("text/uuid");

      // Check if item was dropped below the list items, in the extra space we
      // leave at bottom of the ol.todo
      let toID = (event.target.className === "todo") 
        ? "bottom" : event.target.id;

      if (fromID !== toID && !isDraggedOverNext(event, items)) {
        setItems(prevItems => moveWithinArray(prevItems, fromID, toID));
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
      <ListItem key={item.id} {...props}>{item}</ListItem>
    );
  }

  let [done, notDone] = Utilities.partition(items, (i => i.isDone));

  const ItemsDone = () => done.map(makeListItem);
  const ItemsNotDone= () => notDone.map(makeListItem);

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

export default TaskList;

