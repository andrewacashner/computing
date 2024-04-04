import { Link } from "react-router-dom";

export default function TimelineList({ data }) {
  function link(item) {
    return(
      <li key={item.id}>
        <Link to={`../game/${item.id}`}>{item.title}</Link>
      </li>
    );
  }

  return(
    <ul>
      {data.map(link)}
    </ul>
  );
}
