import { useState } from "react";
import { ToDoItem } from "../classes/ToDoItem";

function ListItem(props) {
  let items = props.itemState.obj;
  let setItems = props.itemState.fn;
  let setFormDefaults = props.formState.fn;
  let item = props.children;

  function toggleDoneStatus() {
    let toggledItem = ToDoItem.toggled(item);
    console.log(`Marking item as ${toggledItem.doneStatus}`);

    let split = items.indexOf(item);
    let before = items.slice(0, split);
    let after = items.slice(split + 1);
    let newItems = [...before, toggledItem, ...after]
    setItems(newItems);
  }

  function dragListItem(event) {
    event.dataTransfer.setData("text/uuid", event.target.id);
  }

  function dragleaveListItem(event) {
    event.preventDefault();
    event.target.classList.remove("gapAbove");
  }
  
  let [isButtonShown, setIsButtonShown] = useState(false);
  let listButtonVisibility = (isButtonShown) ? "show" : "hide";

  const showButton = () => setIsButtonShown(true);
  const hideButton = () => setIsButtonShown(false);

  function EditButton() {
    function editItem(event) {
      setFormDefaults({
        task: item.task,
        deadline: item.deadline
      });
      setItems(items.filter(i => i !== item));
      event.stopPropagation();
    }

    return(
      <button type="button" 
              className={listButtonVisibility}
              onClick={editItem}>🖉</button>
    );
  }

  function DeleteButton() {
    function deleteItem(event) {
      setItems(items.filter(i => i !== item));
      console.log(`Deleting item (task: ${item.task})`);
      event.stopPropagation();
    }

    return(
      <button type="button" 
              className={listButtonVisibility} 
              onClick={deleteItem}>🗙</button>
    );
  }
  
  if (item) {
    let ToDoSpan = item.Span();

    // Cancellation X is U+1F5D9
    // Edit pencil is U+1F589
    return (
      <li key={item.id}
          id={item.id}
          className={`${item.doneStatus} ${item.activeStatus}`}
          onClick={toggleDoneStatus}
          draggable="true"
          onDragStart={dragListItem}
          onDragLeave={dragleaveListItem}
          onMouseEnter={showButton}
          onMouseLeave={hideButton}>
        <ToDoSpan />
        <EditButton />
        <DeleteButton />
      </li>
    );
  }
}



export default ListItem;
