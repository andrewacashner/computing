import { createContext } from "react";
import Game from "../classes/Game";

const TimelineContext = createContext(new Game());

export default TimelineContext;
