import { Outlet } from "react-router-dom";

import Navigation from "./Navigation";
import Footer from "./Footer";

export default function Layout() {
  return(
    <>
      <header>
        <h1>To Do</h1>
        <Navigation />
      </header>
      <Outlet />
      <Footer />
    </>
  );
}
