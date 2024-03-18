import "./App.css";
import { 
  createBrowserRouter, 
  createRoutesFromElements,
  Route,
  RouterProvider 
} from "react-router-dom";

import Layout from "./components/shared/Layout";

import About from "./routes/About";
import ToDo, { loader as toDoLoader} from "./routes/ToDo";
import LogIn from "./routes/LogIn";

const router = createBrowserRouter(
  createRoutesFromElements(
    <Route path="/" element={<Layout />}>
      <Route path="/about" element={<About />} />
      <Route path="/todo" element={<ToDo />} loader={toDoLoader} />
      <Route path="/login" element={<LogIn />} />
    </Route>
  )
);

function App() {
  return(
    <RouterProvider router={router} />
  );
}

export default App;


