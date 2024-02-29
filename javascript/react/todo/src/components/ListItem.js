import { useContext, useState } from "react";
import ToDoContext from "../store/ToDoContext";

function ListItem(props) {
  let item = props.children;
  
  let context = useContext(ToDoContext);
  let setItems = context.items.set;
  let setDraft = context.form.set;

  function toggleDoneStatus() {
    setItems(prevItems => prevItems.toggleDoneStatus(item));
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
      setDraft(prevDraft => item.clone());
      setItems(prevItems => prevItems.removeItem(item));
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
      setItems(prevItems => prevItems.removeItem(item));
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
