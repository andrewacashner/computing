import { NavLink } from "react-router-dom";

export default function Navigation() {
  return(
    <nav>
      <ul>
        <li><NavLink to="/">Home</NavLink></li>
        <li><NavLink to="/about">About</NavLink></li>
        <li><NavLink to="/admin">Account</NavLink></li>
        <li><NavLink to="/game">Play</NavLink></li>
      </ul>
    </nav>
  );
}

