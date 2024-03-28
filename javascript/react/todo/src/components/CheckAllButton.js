import { useContext, useState, useEffect } from "react";
import ToDoContext from "../store/ToDoContext";
import UserContext from "../store/UserContext";

import HttpRequest from "../classes/HttpRequest";

function CheckAllButton(props) {

  let userContext = useContext(UserContext);
  let userToken = userContext.userToken[0];

  let todoContext = useContext(ToDoContext);
  let items = todoContext.get
  let setItems = todoContext.set;

  let [newStatus, setNewStatus] = useState(null);

  function setAllItemStatus(isDone) {
    setNewStatus(isDone);
  }

  useEffect(() => {
    async function doSetStatus(isDone, token) {
      try {
        let request = new HttpRequest({
          method: "POST",
          url: "todo/set_all_status/",
          errorMsg: `Problem setting done status of all items to ${isDone}`,
          bodyObject: { status: isDone },
          authToken: token
        });
        let response = await request.send();
        if (response.ok) {
          let json = await response.json();
          console.log(json);
          setItems(prevItems => prevItems.setAllItemStatus(isDone));
          setNewStatus(null);
        } else {
          throw new Error(request.error(response));
        }
      } catch(e) {
        console.error(e);
      }
    }
    if (newStatus !== null) {
      doSetStatus(newStatus, userToken);
    }
  }, [newStatus, userToken, setItems])


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


 
  let [doSort, setDoSort] = useState(false);

  function sortItemsByDate(items) {
    setDoSort(true);
  }

  useEffect(() => {
    async function backendSort(theseItems, token) {
      try {
        let request = new HttpRequest({
          method: "POST",
          url: "todo/sort_by_date/",
          errorMsg: "Problem sorting all items by date",
          bodyObject: theseItems,
          authToken: token
        });
        let response = await request.send();
        if (response.ok) {
          let json = await response.json();
          console.log(json);
          setItems(prevItems => prevItems.toSortedByDate());
          setDoSort(false);
        } else {
          throw new Error(request.error(response));
        }
      } catch(e) {
        console.error(e);
      }
    }
    if (doSort) {
      backendSort(items, userToken);
    }
  }, [doSort, setItems, items, userToken]);

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
