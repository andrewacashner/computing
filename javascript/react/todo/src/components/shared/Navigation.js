import { NavLink } from "react-router-dom";

export default function Navigation() {
  return(
    <nav>
      <ul>
        <li><NavLink to="/">Home</NavLink></li>
        <li><NavLink to="/about">About</NavLink></li>
        <li><NavLink to="/login">Log In</NavLink></li>
        <li><NavLink to="/todo">Your To Do List</NavLink></li>
      </ul>
    </nav>
  );
}
