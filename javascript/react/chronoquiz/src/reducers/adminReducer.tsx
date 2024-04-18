const defaultAdminState = {
  timelineID:         null,
  updateTimelineList: true
}

function adminReducer(state, action) {
  let obj = action.payload;
  let newState = null;

  switch(action.type) {
    case "set":
      newState = { ...state, ...obj };
    break;

    case "reset":
      newState = defaultAdminState;
    break;

    default:
      newState = state;
  }
  return newState;
}

export { adminReducer, defaultAdminState };
