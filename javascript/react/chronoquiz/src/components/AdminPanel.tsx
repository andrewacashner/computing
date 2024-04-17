import { useState, useContext, useEffect, useReducer } from "react";
import { useNavigate } from "react-router-dom";

import debug from "../lib/debug";

import User from "../classes/User";
import Timeline from "../classes/Timeline";

import UserContext from "../store/UserContext";

import UploadForm from "./UploadForm";

import { timelineReducer, defaultTimeline } from "../reducers/timelineReducer";
import { factReducer, defaultFact } from "../reducers/factReducer";
import updateReducer from "../reducers/updateReducer";

export default function AdminPanel() {
  let userContext = useContext(UserContext);
  let authenticated = userContext.get.authenticated;
  let currentUser   = userContext.get.currentUser;
  let userToken     = userContext.get.userToken;

  // Current timeline on client side
  let [timelineState, dispatchTimeline] = useReducer(timelineReducer, defaultTimeline);

  // Current new fact card
  let [factState, dispatchFact] = useReducer(factReducer, defaultFact);

  // Set a single state field from form input
  const updateTimeline = updateReducer(dispatchTimeline);
  const updateFact = updateReducer(dispatchFact);

  // Initial timeline loaded
  let [initialTimeline, setInitialTimeline] = useState(defaultTimeline);


  // Has the user requested to save the timeline (that is, to send client-side
  // timeline data to the backend database?)
  let [saveReady, setSaveReady] = useState(false);

  // Used to trigger update of timeline display
  // TODO is this needed?
  let [refresh, setRefresh] = useState(true);

  // MONITOR FOR UNSAVED CHANGES
  // Is the client-side timeline data different from what was originally
  // received from the backend server? Used to set display of "save" and
  // "discard changes" button and to prompt to save unsaved changes when new
  // timeline is selected
  let [hasUnsavedChanges, setHasUnsavedChanges] = useState(false);

  // When a timeline is loaded or changed, record whether there are unsaved
  // changes (current timeline differs from initial timeline loaded)
  useEffect(() => {
    if (initialTimeline) { debug(initialTimeline); }
    if (timelineState) { debug(timelineState); }
    
    let status = (timelineState && initialTimeline
                  && !timelineState.equals(initialTimeline));

    setHasUnsavedChanges(status);
  }, [timelineState, initialTimeline]);


  // GET LIST OF USER TIMELINES
  let [timelineList, setTimelineList] = useState([]);
  
  // Do we need to update the list of user timelines in the select options?
  let [updateTimelineList, setUpdateTimelineList] = useState(true);
  
  useEffect(() => {
    async function loadTimelineList(user: User, token: string): void {
      let list = await user.loadUserTimelineList(token);
      if (list) {
        setTimelineList(list);
      }
    }

    if (authenticated && updateTimelineList) {
      loadTimelineList(currentUser, userToken);
      setUpdateTimelineList(false);
    } else {
      setTimelineList([]);
    }
  }, [authenticated, updateTimelineList, currentUser, userToken]);


  // LOAD A TIMELINE FROM BACKEND

  // 'id' field of Javascript Timeline object on client-side, and of Django
  // Timeline model on server-side (= primary key)
  let [timelineID, setTimelineID] = useState(null);

  useEffect(() => {
    async function loadTimeline(user: User, id: number, token: string): void {
      debug(`Loading timeline id ${timelineID}`);

      let newTimeline = await user.fetchTimeline(id, token);
      if (newTimeline) {
        dispatchTimeline({ type: "set", payload: newTimeline });
        setInitialTimeline(newTimeline);
      } 
    }

    if (timelineID) {
      if (!hasUnsavedChanges 
            || window.confirm("Your quiz has unsaved changes. Do you want to discard the changes and reload the quiz?")) {

        if (timelineID === "create") {
          dispatchTimeline({ type: "reset" });
        } else {
          loadTimeline(currentUser, timelineID, userToken);
        }
        setUpdateTimelineList(true);
        setRefresh(false);
        setHasUnsavedChanges(false);
      }
    }
  }, [timelineID, currentUser, userToken, refresh]);

  function PageInstructions() {
    return(
      <p className="instructions">Your data will not be saved until you click Save.</p>
    );
  }

  function Chooser() {
    function loadTimeline(event) {
      event.preventDefault();
      let data = new FormData(event.target);
      let id = data.get("select-timeline");
      setTimelineID(id);
    }

    function timelineOption(timeline) {
      return(
        <option key={timeline.id} value={timeline.id}>{timeline.title}</option>
      );
    }

    let [currentTimelineSelection, setCurrentTimelineSelection] = useState("create");

    function updateSelection(event) {
      setCurrentTimelineSelection(event.target.value);
    }

    let loadButtonText = (currentTimelineSelection === "create") 
                          ? "Create" : "Load";

    return(
      <form id="chooser" onSubmit={loadTimeline}>
        <label htmlFor="select-timeline">Select a Timeline:</label>
        <select name="select-timeline" defaultValue="create" onChange={updateSelection}>
          <option value="create">Create New</option>
          { timelineList.map(timelineOption) }
        </select>
        <button type="submit">{ loadButtonText }</button>
      </form>
    );
  }

  function MetadataPanel() {
    let creator = (timelineState.creator === "") 
                  ? currentUser.username 
                  : timelineState.creator;

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
                defaultValue={creator}/>
            </div>
          </div>
        </form>
      </section>
    );
  }

  function CurrentFactsPanel() {
    function currentFact(item) {
      function deleteFact(event) {
        if (window.confirm("Are you sure you want to delete the current fact?")) {
          debug(`Delete item (date ${item.date.getFullYear()})`);
          dispatchTimeline({ 
            type: "removeFact",
            payload: { fact: item }
          });
        }
      }

      function editFact(event) {
        debug(`Edit item (date ${item.date.getFullYear()})`);
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
          <td>
            <div className="FactListControls">
              <button type="button" onClick={editFact}>Edit</button>
              <button type="button" onClick={deleteFact}>Delete</button>
            </div>
          </td>
          <td>{String(item.year)}</td>
          <td>{item.info}</td>
          <td>{item.img}</td>
        </tr>
      );
    }

    function TimelineInstructions() {
      return(
        <>
          <p className="instructions">Enter timeline events manually or upload the data using the forms below</p>
          <UploadForm dispatch={dispatchTimeline} />
        </>
      );
    }

    return(
      <section id="currentTimeline">
        <h2>Current Timeline Events</h2>
        <TimelineInstructions />
        <table className="timeline">
          <thead>
            <tr>
              <th>Controls</th>
              <th>Year</th>
              <th>Description</th>
              <th>Image URL</th>
            </tr>
          </thead>
          <tbody>
            { timelineState.facts.map(currentFact) }
          </tbody>
        </table>
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
        debug("Added fact to timeline");
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
                max={defaultFact.year}
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

  function activeStyle(isActive) {
    return isActive ? "active" : "inactive";
  }
  
  function SaveButton() {
    return(
      <button className={activeStyle(hasUnsavedChanges)} id="save" type="button" onClick={saveTimeline}>Save</button>
    );
  }


  // TODO indicate save state: 
  // (1) metadata, (2) facts, (3) whole timeline on server
  function saveTimeline(event) {
    let action = timelineState ? "Updated" : "Created";
    debug(`${action} timeline with title '${timelineState.title}'`);
    setSaveReady(true);
  }

  useEffect(() => {
    async function postTimeline(user, token, timeline) {
      debug(timeline.facts);
      debug(timeline.json());
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
        debug(json);
      } else {
        debug(`Problem creating timeline: Server status ${response.status}, ${response.statusText}`);
      }
    }
    if (saveReady) {
      debug("Ready to post timeline");
      debug(timelineState);
      postTimeline(currentUser, userToken, timelineState);
      setSaveReady(false);
      setUpdateTimelineList(true);
      setHasUnsavedChanges(false);
    } 
  }, [saveReady, timelineState, currentUser, userToken]);
  

  function DeleteTimelineButton() {
    let [timelineToDelete, setTimelineToDelete] = useState(null);

    function deleteTimeline() {
      if (window.confirm("Are you sure you want to delete this quiz? All of its fact cards will be lost. This action cannot be undone.")) {
        debug(timelineID);
        setTimelineToDelete(timelineID);
      }
    }

    useEffect(() => {
      async function requestDeletion(timelineID, token) {
        let result = false;
        let response = await fetch(`${User.SERVER}/timelines/${timelineID}/`, {
          method: "DELETE",
          headers: new Headers({
            "Accept": "application/json",
            "Authorization": `Token ${token}`
          })
        });
        if (response.ok) {
          let json = await response.json();
          debug(json);
          result = true;
        } else {
          debug(`Could not delete timeline with id ${timelineID}: Server status ${response.status}, ${response.statusText}`);
          result = false;
        }
        return result;
      }

      if (timelineToDelete !== null) {
        debug(`Deleting timeline with id ${timelineToDelete}`);

        if (timelineID === "create") {
          dispatchTimeline({ type: "reset" });
        } else {
          let deleted = requestDeletion(timelineToDelete, userToken);
          if (deleted) {
            dispatchTimeline({ type: "reset" });
            setUpdateTimelineList(true);
          }
        }

        setTimelineToDelete(null);
      }
    }, [timelineToDelete]);

    let msg = (timelineID === "create") ? "Reset Quiz" : "Delete Quiz";
    return(
      <button id="deleteTimeline" type="button" onClick={deleteTimeline}>{ msg }</button>
    );
  }


  function DiscardChangesButton() {
    function discardChanges(event) {
      setRefresh(true);
    }

    return(
      <button type="button" className={activeStyle(hasUnsavedChanges)} onClick={discardChanges}>Discard Changes</button>
    );
  }

  function Controls() {
    return(
      <div className="controls">
        <SaveButton />
        <DiscardChangesButton />
        <DeleteTimelineButton />
      </div>
    );
  }
  let navigate = useNavigate();

  if (authenticated) {
    return(
      <main>
        <h1>Manage Your Quizzes</h1>
        <PageInstructions />
        <Chooser />
        <MetadataPanel />
        <CurrentFactsPanel />
        <NewFactForm />
        <Controls />
      </main>
    );
  } else {
    navigate("/login");
  }
}

