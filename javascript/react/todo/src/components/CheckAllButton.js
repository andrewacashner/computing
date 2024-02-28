import { useContext } from "react";
import ToDoContext from "../store/ToDoContext";
import ToDoItem from "../classes/ToDoItem";
import ToDoList from "../classes/ToDoList";

function CheckAllButton(props) {
  let context = useContext(ToDoContext);
  let items = context.items.get
  let setItems = context.items.set;

  function setAllItemStatus(isDone) {
    setItems(prevItems => prevItems.setAllItemStatus(isDone));
  }

  const uncheckAll = () => setAllItemStatus(false);
  const checkAll = () => setAllItemStatus(true);
 
  let uncheckAllStatus = items.areAnyDone();
  let checkAllStatus = items.areAnyLeftToDo();

  function sortItemsByDate(items) {
    setItems(prevItems => prevItems.toSortedByDate());
  }

  const clearAll = () => setItems(new ToDoList());

  if (items.list.length > 0) {
    return(
      <div className="todoControls">
        <button type="button" 
                onClick={checkAll}
                className={ToDoItem.activeOrNot(!checkAllStatus)}
        >Mark all as finished</button>
        <button type="button" 
                onClick={uncheckAll}
                className={ToDoItem.activeOrNot(!uncheckAllStatus)}
        >Mark all as unfinished</button>
        <button type="button" 
                onClick={sortItemsByDate}
                className={ToDoItem.activeOrNot(items.isSorted())}
        >Sort by date</button>
        <button type="button" onClick={clearAll}>Clear all</button>
      </div>
    );
  }
}

export default CheckAllButton;
