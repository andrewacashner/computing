import { useContext } from "react";
import ToDoContext from "../store/ToDoContext";
import { ToDoItem, ToDoList } from "../classes/ToDoItem";

function CheckAllButton(props) {
  let context = useContext(ToDoContext);
  let items = context.items.get
  let setItems = context.items.set;

  function setAllItemStatus(isDone) {
    setItems(prevItems => ToDoList.setAllItemStatus(prevItems, isDone));
  }

  const checkAll = () => setAllItemStatus(true);
  const uncheckAll = () => setAllItemStatus(false);
 
  let checkAllStatus = ToDoList.areAnyLeftToDo(items);
  let uncheckAllStatus = ToDoList.areAnyDone(items);

  const sortItemsByDate = () => setItems(ToDoList.toSortedByDate);
  const clearAll = () => setItems([]);

  if (items.length > 0) {
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
                className={ToDoItem.activeOrNot(ToDoList.isSorted(items))}
        >Sort by date</button>
        <button type="button" onClick={clearAll}>Clear all</button>
      </div>
    );
  }
}

export default CheckAllButton;
