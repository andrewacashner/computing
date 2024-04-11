import { useState, useContext, useEffect, useCallback } from "react";
import { useNavigate } from "react-router-dom";

import User from "../classes/User";
import Fact from "../classes/Fact";
import Timeline from "../classes/Timeline";

import UserContext from "../store/UserContext";

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

  const startingTimeline = useCallback(() => {
    return new Timeline({
      creator: currentUser.username
    });
  }, [currentUser.username]);

  let [timeline, setTimeline] = useState(startingTimeline());
  console.debug(timeline);

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
        let newTimeline = new Timeline({
          id: json.id,
          title: json.title,
          description: json.description,
          keywords: Timeline.parseKeywords(json.keywords),
          creator: Timeline.creator ?? currentUser.username,
          facts: json.facts.map(f => Fact.newFromYear(f))
        });
        setTimeline(newTimeline);
      } else {
        console.debug(`Problem loading timeline with id ${id}: Server status ${response.status}, ${response.statusText}`);
      }
    }

    if (timelineID) {
      if (timelineID === "create") {
        setTimeline(startingTimeline());
      } else {
        loadTimeline(timelineID, userToken);
      }
    }
  }, [timelineID, currentUser.username, startingTimeline, userToken]);


  function Chooser() {
    function timelineOption(timeline) {
      return(
        <option key={timeline.id} value={timeline.id}>{timeline.title}</option>
      );
    }
    return(
      <form id="chooser" onSubmit={loadTimeline}>
        <label for="select-timeline">Select a Timeline:</label>
        <select name="select-timeline" defaultValue="create">
          <option value="create">Create New</option>
          { timelineList.map(timelineOption) }
        </select>
        <button type="submit">Submit</button>
      </form>
    );
  }

  function MetadataPanel() {

    function setTitle(event) {
      setTimeline(prev => new Timeline({
        ...prev,
        title: event.target.value
      }));
    }

    function setDescription(event) {
      setTimeline(prev => new Timeline({
        ...prev,
        description: event.target.value
      }));
    }

    function setKeywords(event) {
      setTimeline(prev => new Timeline({
        ...prev,
        keywords: Timeline.parseKeywords(event.target.value)
      }));
    }

    function setCreator(event) {
      setTimeline(prev => new Timeline({
        ...prev,
        creator: event.target.value
      }));
    }

    return(
      <form className="timelinePanel">
        <div className="formInputBlock">
          <div className="formItem">
            <label htmlFor="title">Title</label>
            <input type="text" name="title" 
              onBlur={setTitle}
              defaultValue={timeline.title} />
          </div>
          <div className="formItem">
            <label htmlFor="description">Description</label>
            <input type="text" name="description" 
              onBlur={setDescription}
              defaultValue={timeline.description} />
          </div>
          <div className="formItem">
            <label htmlFor="keywords">Keywords (separated with semicolons)</label>
            <input type="text" name="keywords" 
              onBlur={setKeywords}
              defaultValue={timeline.keywordString} />
          </div>
          <div className="formItem">
            <label htmlFor="creator">Creator (for public display; default: your username)</label>
            <input type="text" name="creator" 
              onBlur={setCreator}
              defaultValue={timeline.creator}/>
          </div>
        </div>
      </form>
    );
  }

  function CurrentFactsPanel() {

    function currentFact(item) {

      function deleteFact(event) {
        if (window.confirm("Are you sure you want to delete the current fact? Deleted facts can be recovered by reloading the timeline without first clicking Save.")) {
          console.debug(`Delete item (date ${item.date.getFullYear()})`);
          setTimeline(prev => prev.removeFact(item));
        }
      }

      function editFact(event) {
        console.debug(`Edit item (date ${item.date.getFullYear()})`);
        setTestCard(item);
        setTimeline(prev => prev.removeFact(item));
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
            { timeline ? timeline.facts.map(currentFact) : null }
          </tbody>
        </table>
        { timeline ? null : <Instructions /> }
      </section>
    );
  }

  const startingCard = () => new Fact({
    date: new Date(),
    info: "Description", 
    img: "https://picsum.photos/200.jpg"
  });

  let [testCard, setTestCard] = useState(startingCard());

  function NewFactForm() {

    function setTimelineFacts(facts: Array<Fact>) {
      let newTimeline = new Timeline({
        ...timeline,
        facts: facts
      });
      newTimeline.sortByDate();
      setTimeline(newTimeline);
    }

    function newFact(event) {
      if (testCard.date && testCard.info) {
        setTimelineFacts([...timeline.facts, testCard]);
        console.debug("Added fact to timeline");
        setTestCard(startingCard());
      }
    }

    function CardPreview({ fact }) {
      return(
        <div className="card" data-when={fact.year} data-noselect="noselect">
          <span className="date">{fact.year}</span>
          <img alt={fact.img} src={fact.img} />
          <span className="info">{fact.info}</span>
        </div>
      );
    }

    function setDate(event) {
      let newDate = new Date();
      newDate.setFullYear(Number(event.target.value));
      setTestCard(prev => new Fact({ ...prev, date: newDate }));
    }

    function setInfo(event) {
      let newInfo = event.target.value;
      setTestCard(prev => new Fact({ ...prev, info: newInfo }));
    }

    function setImg(event) {
      let newImg = event.target.value;
      setTestCard(prev => new Fact({ ...prev, img: newImg }));
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
                max={startingCard().year}
                onChange={setDate}
                defaultValue={testCard.year} />
            </div>
            <div className="formItem">
              <label htmlFor="info">Description of event</label>
              <input 
                type="text" 
                name="info" 
                onBlur={setInfo}
                defaultValue={testCard.info} />
            </div>
            <div className="formItem">
              <label htmlFor="img">Complete URL of image (optional)</label>
              <input 
                type="url" 
                name="img" 
                onBlur={setImg}
                defaultValue={testCard.img} />
            </div>
          </div>
          <section id="preview">
            <h3>Preview</h3>
            <CardPreview fact={testCard} />
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

 
  function saveTimeline(event) {
    let action = timeline ? "Updated" : "Created";
    console.debug(`${action} timeline with title '${timeline.title}'`);

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
      setTimeline(startingTimeline());
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
      console.debug(timeline);
      postTimeline(currentUser, userToken, timeline);
      setSaveReady(false);
    } 
  }, [saveReady, timeline, currentUser, userToken]);


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

