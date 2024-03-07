export default function Card(props) {
  let card = props.children;
  return(
    <div key={card.id}
      className="card" 
      id={card.id}
      data-when={card.year}
      data-noselect="noselect">
      <span className="date">{card.dateToString()}</span>
      <img alt="Clue" src={card.img} />
      <span className="info">{card.info}</span>
    </div>
  );
}
