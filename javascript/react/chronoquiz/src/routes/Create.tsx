import { useState, useContext, useEffect, useReducer } from "react";
import { useNavigate } from "react-router-dom";

import User from "../classes/User";
import Fact from "../classes/Fact";
import Timeline from "../classes/Timeline";

import UserContext from "../store/UserContext";

const timelineDefaults = new Timeline({
  title: '',
  description: '',
  keywordString: [],
  creator: currentUser.username,
  facts: []
});

function timelineReducer(state, action) {
  let toDo = action.type;
  let obj = action.payload;

  let newState = state;

  const actions = {
    set:        new Timeline({ ...state, ...obj }),
    reset:      timelineDefaults,
    addFact:    state.addFact(obj.fact),
    removeFact: state.removeFact(obj.fact),
  };

  if (toDo in actions) {
    newState = actions[toDo];
  }
  return newState;
}


const factDefaults = new Fact({
  date: new Date(),
  info: "Description", 
  img: "https://picsum.photos/200.jpg"
});

function factReducer(state, action) {
  let toDo = action.type;
  let obj = action.payload;
  let newState = state;

  const actions = {
    set:    new Fact({ ...state, ...obj }),
    reset:  factDefaults
  };

  if (toDo in actions) {
    newState = actions[toDo];
  }
  return newState;
}

function updateReducer(dispatchFn: (obj: object) => obj) {
  return function(
    field: string, 
    setValueFn: (value: any) => any = null
  ): void {
    return function(event): void {
      let value = event.target.value;
      let newValue = setValueFn ? setValueFn(value) : value;
      dispatchFn({
        type: "set",
        payload: { 
          [field]: newValue
        }
      });
    };
  }
}


export default function Create() {
  let userContext = useContext(UserContext);
  let authenticated = userContext.get("authenticated");
  let currentUser = userContext.get("currentUser");
  let userToken = userContext.get("userToken");

  let navigate = useNavigate();

  useEffect(() => {
    if (!authenticated) {
      navigate("../admin");
    }
  }, [authenticated, navigate]);


  let [timelineState, dispatchTimeline] = useReducer(timelineReducer, timelineDefaults);
  let [factState, dispatchFact] = useReducer(factReducer, factDefaults);
  
  const updateFact = updateReducer(dispatchFact);
  const updateTimeline = updateReducer(dispatchTimeline);

  let [saveReady, setSaveReady] = useState(false);
  
  function Instructions() {
    return(
      <p className="instructions">Your data will not be saved until you click Save.</p>
    );
  }

  let [timelineList, setTimelineList] = useState([]);

  useEffect(() => {
    async function loadTimelineList(user, token) {
      let list = await user.loadUserTimelineList(token);
      if (list) {
        setTimelineList(list);
      }
    }
    if (authenticated) {
      loadTimelineList(currentUser, userToken);
    } else {
      setTimelineList([]);
    }
  }, [authenticated, currentUser, userToken]);

  let [timelineID, setTimelineID] = useState(null);

  function loadTimeline(event) {
    event.preventDefault();
    let data = new FormData(event.target);
    let id = data.get("select-timeline");
    setTimelineID(id);
  }

  useEffect(() => {
    async function loadTimeline(id, token) {
      let response = await fetch(`${User.SERVER}/timeline-full/${id}`, {
        method: "GET",
        headers: new Headers({
          "Authorization": `Token ${token}`
        })
      });

      if (response.ok) {
        let json = await response.json();
        console.debug(json);

        dispatchTimeline({
          type: "set",
          payload: {
            id:           json.id,
            title:        json.title,
            description:  json.description,
            keywords:     Timeline.parseKeywords(json.keywords),
            creator:      Timeline.creator ?? currentUser.username,
            facts:        json.facts.map(f => Fact.newFromYear(f))
          }
        });
      } else {
        console.debug(`Problem loading timeline with id ${id}: Server status ${response.status}, ${response.statusText}`);
      }
    }

    if (timelineID) {
      if (timelineID === "create") {
        dispatchTimeline({ type: "reset" })
      } else {
        loadTimeline(timelineID, userToken);
      }
    }
  }, [timelineID, currentUser.username, userToken]);


  function Chooser() {
    function timelineOption(timeline) {
      return(
        <option key={timeline.id} value={timeline.id}>{timeline.title}</option>
      );
    }
    return(
      <form id="chooser" onSubmit={loadTimeline}>
        <label htmlFor="select-timeline">Select a Timeline:</label>
        <select name="select-timeline" defaultValue="create">
          <option value="create">Create New</option>
          { timelineList.map(timelineOption) }
        </select>
        <button type="submit">Submit</button>
      </form>
    );
  }

  function MetadataPanel() {

    return(
      <section id="metadata">
        <h2>Metadata</h2>
        <form className="timelinePanel">
          <div className="formInputBlock">
            <div className="formItem">
              <label htmlFor="title">Title</label>
              <input type="text" name="title" 
                onBlur={updateTimeline("title")}
                defaultValue={timelineState.title} />
            </div>
            <div className="formItem">
              <label htmlFor="description">Description</label>
              <input type="text" name="description" 
                onBlur={updateTimeline("description")}
                defaultValue={timelineState.description} />
            </div>
            <div className="formItem">
              <label htmlFor="keywords">Keywords (separated with semicolons)</label>
              <input type="text" name="keywords" 
                onBlur={updateTimeline("keywords", Timeline.parseKeywords)}
                defaultValue={timelineState.keywordString} />
            </div>
            <div className="formItem">
              <label htmlFor="creator">Creator (for public display; default: your username)</label>
              <input type="text" name="creator" 
                onBlur={updateTimeline("creator")}
                defaultValue={timelineState.creator}/>
            </div>
          </div>
        </form>
      </section>
    );
  }

  function CurrentFactsPanel() {
    console.debug(timelineState);

    function currentFact(item) {
      function deleteFact(event) {
        if (window.confirm("Are you sure you want to delete the current fact? Deleted facts can be recovered by reloading the timeline without first clicking Save.")) {
          console.debug(`Delete item (date ${item.date.getFullYear()})`);
          dispatchTimeline({ 
            type: "removeFact",
            payload: { fact: item }
          });
        }
      }

      function editFact(event) {
        console.debug(`Edit item (date ${item.date.getFullYear()})`);
        dispatchFact({
          type: "set",
          payload: item
        });
        dispatchTimeline({ 
          type: "removeFact",
          payload: { fact: item }
        });
      }

      return(
        <tr key={crypto.randomUUID()}>
          <td><button type="button" onClick={editFact}>Edit</button>
            <button type="button" onClick={deleteFact}>Delete</button></td>
          <td>{item.id}</td>
          <td>{item.year}</td>
          <td>{item.info}</td>
          <td>{item.img}</td>
        </tr>
      );
    }

    function Instructions() {
      return(
        <p className="instructions">Enter timeline events using the form below</p>
      );
    }

    return(
      <section id="currentTimeline">
        <h2>Current Timeline Events</h2>
        <table className="timeline">
          <thead>
            <tr>
              <th>Controls</th>
              <th>ID</th>
              <th>Year</th>
              <th>Description</th>
              <th>Image URL</th>
            </tr>
          </thead>
          <tbody>
            { timelineState.facts.map(currentFact) }
          </tbody>
        </table>
        { timelineState.facts.length === 0 ? <Instructions /> : null }
      </section>
    );
  }

  function NewFactForm() {

    function newFact(event) {
      if (factState.date && factState.info) {
        dispatchTimeline({
          type: "addFact",
          payload: { fact: factState }
        });
        console.debug("Added fact to timeline");
        dispatchFact({ type: "reset" });
      }
    }
    
    function CardPreview({ fact }) {
      return(
        <div className="card" data-when={factState.year} data-noselect="noselect">
          <span className="date">{factState.year}</span>
          <img alt={factState.img} src={factState.img} />
          <span className="info">{factState.info}</span>
        </div>
      );
    }




    function dateFromYear(year: number): Date {
      let date = new Date();
      date.setFullYear(year);
      return date
    }

    return(
      <section id="new">
        <h2>Add an Event</h2>
        <form id="addFactForm">
          <div className="formInputBlock">
            <div className="formItem">
              <label htmlFor="date">Year</label>
              <input 
                type="number" 
                name="date" 
                max={factDefaults.year}
                onChange={updateFact("date", dateFromYear)}
                defaultValue={factState.year} />
            </div>
            <div className="formItem">
              <label htmlFor="info">Description of event</label>
              <input 
                type="text" 
                name="info" 
                onBlur={updateFact("info")}
                defaultValue={factState.info} />
            </div>
            <div className="formItem">
              <label htmlFor="img">Complete URL of image (optional)</label>
              <input 
                type="url" 
                name="img" 
                onBlur={updateFact("img")}
                defaultValue={factState.img} />
            </div>
          </div>
          <section id="preview">
            <h3>Preview</h3>
            <CardPreview fact={factState} />
          </section>
          <button type="button" id="add" onClick={newFact}>Add</button>
        </form>
      </section>
    );
  }

  function SaveButton() {
    return(
      <button id="save" type="button" onClick={saveTimeline}>Save</button>
    );
  }


  // TODO indicate save state: 
  // (1) metadata, (2) facts, (3) whole timeline on server
  function saveTimeline(event) {
    let action = timelineState ? "Updated" : "Created";
    console.debug(`${action} timeline with title '${timelineState.title}'`);
    setSaveReady(true);
  }

  function ResetButton() {
    return(
      <button id="reset" type="button" onClick={resetTimeline}>Reset</button>
    );
  }

  // TODO reset to last saved version on backend
  function resetTimeline() {
    if (window.confirm("Are you sure you want to discard changes to the current timeline? This action cannot be undone.")) {
      dispatchTimeline({ type: "reset" });
    }
  }

   
  useEffect(() => {
    async function postTimeline(user, token, timeline) {
      console.debug(timeline.facts);
      console.debug(timeline.json());
      let response = await fetch(`${User.SERVER}/timeline-full/`, {
        method: "POST",
        headers: new Headers({
          "Content-Type": "application/json",
          "Accept": "application/json",
          "Authorization": `Token ${token}`
        }),
        body: timeline.json()
      });
      if (response.ok) {
        let json = await response.json();
        console.debug(json);
      } else {
        console.debug(`Problem creating timeline: Server status ${response.status}, ${response.statusText}`);
      }
    }
    if (saveReady) {
      console.debug("Ready to post timeline");
      console.debug(timelineState);
      postTimeline(currentUser, userToken, timelineState);
      setSaveReady(false);
    } 
  }, [saveReady, timelineState, currentUser, userToken]);


  return(
    <main>
      <h1>Create a Chronoquiz</h1>
      <Instructions />
      <Chooser />
      <MetadataPanel />
      <CurrentFactsPanel />
      <NewFactForm />
      <SaveButton />
      <ResetButton />
    </main>
  );
}

