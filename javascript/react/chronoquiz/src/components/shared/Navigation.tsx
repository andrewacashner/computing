import { useContext } from "react";
import { NavLink } from "react-router-dom";

import UserContext from "../../store/UserContext";

export default function Navigation() {
  let userContext = useContext(UserContext);
  let authenticated = userContext.get("authenticated");

  return(
    <nav>
      <ul>
        <li><NavLink to="/">Home</NavLink></li>
        <li><NavLink to="/about">About</NavLink></li>
        { authenticated 
          ? <li><NavLink to="/login">Log Out</NavLink></li>
          : <li><NavLink to="/login">Log In</NavLink></li> }
        <li><NavLink to="/chronoquiz">Play</NavLink></li>
      </ul>
    </nav>
  );
}

