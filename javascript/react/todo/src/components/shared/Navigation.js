import { NavLink } from "react-router-dom";

export default function Navigation() {
  return(
    <nav>
      <ul>
        <li><NavLink to="/">Home</NavLink></li>
        <li><NavLink to="/about">About</NavLink></li>
        <li><NavLink to="/todo">Make a list without logging in</NavLink></li>
        <li><NavLink to="/login">Log In</NavLink></li>
      </ul>
    </nav>
  );
}