import { createContext } from "react";
import Game from "../classes/Game";

const TimelineContext = createContext(new Game(Game.startingGame()));

export default TimelineContext;
