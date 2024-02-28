import { ToDoItem } from "../classes/ToDoItem";

function CheckAllButton(props) {
  let items = props.itemState.obj;
  let setItems = props.itemState.fn;

  function setAllItemStatus(isDone) {
    let newItems = items.map(i => new ToDoItem({...i, isDone: isDone}));
    setItems(newItems);
  }

  const checkAll = () => setAllItemStatus(true);
  const uncheckAll = () => setAllItemStatus(false);
 
  let checkAllStatus = items.some(i => i.isDone === false);
  let uncheckAllStatus = items.some(i => i.isDone === true);

  const sortItemsByDate = () => setItems(items.toSortedByDate());
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
                className={ToDoItem.activeOrNot(items.isSorted())}
        >Sort by date</button>
        <button type="button" onClick={clearAll}>Clear all</button>
      </div>
    );
  }
}

export default CheckAllButton;
