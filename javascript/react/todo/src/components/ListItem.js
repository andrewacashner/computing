import { useContext, useState, useEffect } from "react";
import UserContext from "../store/UserContext";
import ToDoContext from "../store/ToDoContext";

import HttpRequest from "../classes/HttpRequest";

function ListItem(props) {
  let item = props.children;
 
  let userContext = useContext(UserContext);
  let userToken = userContext.userToken[0];

  let todoContext = useContext(ToDoContext);
  let setItems = todoContext.set;

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
      setItems(prevItems => prevItems.moveItemToDraft(item));
      // TODO update db
      event.stopPropagation();
    }

    return(
      <button type="button" 
              className={listButtonVisibility}
              onClick={editItem}>🖉</button>
    );
  }



  function DeleteButton() {
    let [itemToDelete, setItemToDelete] = useState();

    function deleteItem(event) {
      setItemToDelete(item);
      console.log(`Deleting item (task: ${item.task})`);
      console.debug(`Item to delete: ${itemToDelete?.id}`);
      event.stopPropagation();
    }

    useEffect(() => {
      async function sendDeleteRequest(thisItem, token) {
        let request = new HttpRequest({
          method: "POST",
          url: "todo/delete/",
          errorMsg: `Could not delete item with id ${thisItem.id}`,
          bodyObject: thisItem,
          authToken: token
        });
        let response = await request.send();
        if (response.ok) {
          // TODO use response (.json()) body as msg instead?
          console.debug("Successful delete request");
          setItems(prevItems => prevItems.removeItem(item));
        } else {
          throw new Error(request.error(response));
        }
      }
      console.debug(`(useEffect) This Item : ${item?.id}`);
      console.debug(`(useEffect) Item to delete: ${itemToDelete?.id}`);
      if (itemToDelete === item) {
        sendDeleteRequest(itemToDelete, userToken);
      }
    }, [itemToDelete]);

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
