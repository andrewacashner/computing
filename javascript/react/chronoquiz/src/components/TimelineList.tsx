import { Link } from "react-router-dom";

export default function TimelineList({ data, hideUser = false }) {
  function link(item) {
    let username = hideUser ? null : <> (<code>{item.username}</code>)</>;

    return(
      <li key={item.id}>
        <Link to={`../game/${item.id}`}>{item.title}{username}</Link>
      </li>
    );
  }

  return(
    <ul>
      {data.map(link)}
    </ul>
  );
}
