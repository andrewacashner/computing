function color(card) {
  const bg = card => ({ style: { backgroundColor: card.color.css } });
  return (card.isClue) ? null : bg(card);
}

function dragstartHandler(event) {
  event.dataTransfer.setData("id", event.target.id);
  event.dataTransfer.effectAllowed = "move";
}

function draggable(card) {
  const dragSettings = { 
    draggable : true,
    onDragStart: dragstartHandler
  };
  return (card.isClue) ? dragSettings : null;
}

function expand(card) {
  return (card.expand) ? { "data-expand": true } : null;
}

function classList(card) {
  let classes = ["card"];
  if (card.flash) {
    classes.push("flash");
  }
  return classes.join(" ");
}

export default function Card(props) {
  let card = props.children;

  if (card) {
    return(
      <div key={card.id}
        className={classList(card)}
        id={card.id}
        data-when={card.date.getFullYear()}
        data-noselect="noselect"
        {...expand(card)}
        {...color(card)}
        {...draggable(card)}>
        <span className="date">{card.dateToString()}</span>
        { card.img ? <img alt="Clue" src={card.img} /> : null }
        <span className="info">{card.info}</span>
      </div>
    );
  }
}
