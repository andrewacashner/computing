import { useContext } from "react";
import ToDoContext from "../store/ToDoContext";

function CheckAllButton(props) {
  let context = useContext(ToDoContext);
  let items = context.items.get
  let setItems = context.items.set;
  let clearAll = context.items.reset;

  function setAllItemStatus(isDone) {
    setItems(prevItems => prevItems.setAllItemStatus(isDone));
  }

  const uncheckAll = () => setAllItemStatus(false);
  const checkAll = () => setAllItemStatus(true);
 
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
