function color(card) {
  let color = null;
  if (!card.isClue) {
    color = { style: { backgroundColor: card.color.css } };
  }
  return color;
}

export default function Card(props) {
  let card = props.children;

  if (card) {
    return(
      <div key={card.id}
        className="card" 
        id={card.id}
        data-when={card.date}
        data-noselect="noselect"
        {...color(card)}>
        <span className="date">{card.dateToString()}</span>
        <img alt="Clue" src={card.img} />
        <span className="info">{card.info}</span>
      </div>
    );
  }
}
