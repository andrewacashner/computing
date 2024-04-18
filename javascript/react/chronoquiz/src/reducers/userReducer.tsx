import User from "../classes/User";
import debug from "../lib/debug";

const defaultUserState = {
  currentUser:    new User(),
  authenticated:  false,
  userToken:      null,    // Used for token authentication with backend
  timelineList:   []
}

function userReducer(state, action) {
  debug(action);
  debug(state);

  let obj = action.payload;
  let newState = null;

  switch(action.type) {
    case "set":
      newState = { ...state, ...obj };
    break;

    case "user":
      newState = { ...state, currentUser: obj };
    break;

    case "list":
      newState = { ...state, timelineList: [...obj] };
    break;

    case "reset":
      newState = defaultUserState;
    break;

    default:
      newState = state;
  }

  return newState;
}

export { userReducer, defaultUserState };


