import { useContext, useState, useEffect } from "react";
import ToDoContext from "../store/ToDoContext";
import UserContext from "../store/UserContext";

import HttpRequest from "../classes/HttpRequest";

function CheckAllButton(props) {

  let userContext = useContext(UserContext);
  let userToken = userContext.get("currentUser").token;

  let todoContext = useContext(ToDoContext);
  let items = todoContext.get
  let setItems = todoContext.set;

  function setAllItemStatus(isDone) {
    console.log(`Set done status of all items to ${isDone}`);
    setItems(prevItems => prevItems.setAllItemStatus(isDone));
  }

  const uncheckAll = () => setAllItemStatus(false);
  const checkAll = () => setAllItemStatus(true);

  let [clear, setClear] = useState(null);

  function clearAll() {
    setClear(true);
  }
  
  useEffect(() => {
    async function doClearAll(token) {
      try {
        let request = new HttpRequest({
          method: "POST",
          url: "todo/delete_all/",
          errorMsg: "Problem deleting all items",
          authToken: token
        });
        let response = await request.send();
        if (response.ok) {
          let json = await response.json();
          console.log(json);
          todoContext.reset();
          setClear(null);
        } else {
          throw new Error(request.error(response));
        }
      } catch(e) {
        console.error(e);
      }
    }
    if (clear != null) {
      if (window.confirm("Are you sure you want to delete all items? This action cannot be undone.")) {
        doClearAll(userToken);
      } else {
        setClear(null);
      }
    }
  }, [clear, userToken, todoContext])

 
  function sortItemsByDate(items) {
          setItems(prevItems => prevItems.toSortedByDate());
  }

  if (items.list.length > 0) {
    return(
      <div className="todoControls">
        <button type="button" 
                onClick={checkAll}
                className={items.checkAllStatus()}
        >Mark all as finished</button>
        <button type="button" 
                onClick={uncheckAll}
                className={items.uncheckAllStatus()}
        >Mark all as unfinished</button>
        <button type="button" 
                onClick={sortItemsByDate}
                className={items.unsortedStatus()}
        >Sort by date</button>
        <button type="button" onClick={clearAll}>Clear all</button>
      </div>
    );
  }
}

export default CheckAllButton;
