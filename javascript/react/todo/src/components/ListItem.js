import { useState } from "react";
import { ToDoItem } from "../classes/ToDoItem";

function ListItem(props) {
  let setItems = props.items[1];
  let setFormDefaults = props.form[1];
  let item = props.children;

  function toggleDoneStatus() {
    function doToggle(items) {
      let toggledItem = ToDoItem.toggled(item);
      console.log(`Marking item as ${toggledItem.doneStatus}`);

      let split = items.indexOf(item);
      let before = items.slice(0, split);
      let after = items.slice(split + 1);
      return [...before, toggledItem, ...after];
    }
    setItems(doToggle);
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
      
  const removeItem = items => items.filter(i => i !== item);
  
  function EditButton() {
    function editItem(event) {
      setFormDefaults({
        task: item.task,
        deadline: item.deadline
      });
      setItems(removeItem);
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
      setItems(removeItem);
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
