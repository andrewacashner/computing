/*
 * TODO
 * - We can create timeline and add items, but clicking Save sends timeline
 * creation request to server with 0 event items.
 * - On server, create OR update
 */

import { useState, useContext, useEffect } from "react";
import { useNavigate } from "react-router-dom";

import User from "../classes/User";
import Card from "../classes/Card";
import FactList from "../classes/FactList";

import UserContext from "../store/UserContext";

interface TimelineInput {
  title: string;
  description: string;
  keywords: Array<string>;
  creator: string;
  events: FactList;
}

class Timeline {
  title: string;
  description: string;
  keywords: Array<string>;
  creator: string;
  events: FactList;

  constructor({ 
    title, 
    description = "", 
    keywords = [], 
    creator = "", 
    events = new FactList() 
  }: TimelineInput) {
    this.title = title;
    this.description = description;
    this.keywords = keywords;
    this.creator = creator;
    this.events = events;
  }

  static parseKeywords(inputStr: string): Array<string> {
    return inputStr.split(";").map(s => s.trim());
  }

  static keywordString(keywords: Array<string>) {
    return keywords.join("; ");
  }

  addFact(card: Card): Timeline {
    return new Timeline({
      ...this,
      events: new FactList([...this.events.cards, card]).sortByDate()
    });
  }

  json() {
    return JSON.stringify({
      title: this.title,
      description: this.description,
      keywords: this.keywords,
      creator: this.creator,
      events: this.events.json()
     });
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

  let [timeline, setTimeline] = useState(null);
  let [saveReady, setSaveReady] = useState(false);

  function Instructions() {
    return(
      <p className="instructions">Your data will not be saved until you click Save.</p>
    );
  }
  
  let [title, setTitle] = useState("");
  let [description, setDescription] = useState("");
  let [keywords, setKeywords] = useState([""]);
  let [creator, setCreator] = useState(currentUser.username);
  let [events, setEvents] = useState(new FactList());

  function updateTimeline() {
    setTimeline(new Timeline({
      title: title,
      description: description,
      keywords: keywords,
      creator: creator,
      events: events
    }));
  }

  function MetadataPanel() {

    function setField(setFn) {
      return function(event) {
        setFn(event.target.value);
        updateTimeline();
      }
    }

    function setKeywordsParsed(event) {
      setKeywords(Timeline.parseKeywords(event.target.value));
    }

    return(
      <form className="timelinePanel">
        <div className="formInputBlock">
          <div className="formItem">
            <label htmlFor="title">Title</label>
            <input type="text" name="title" 
              onBlur={event => setTitle(event.target.value)}
              defaultValue={title} />
          </div>
          <div className="formItem">
            <label htmlFor="description">Description</label>
            <input type="text" name="description" 
              onBlur={setField(setDescription)}
              defaultValue={description} />
          </div>
          <div className="formItem">
            <label htmlFor="keywords">Keywords (separated with semicolons)</label>
            <input type="text" name="keywords" 
              onBlur={setKeywordsParsed}
              defaultValue={Timeline.keywordString(keywords)} />
          </div>
          <div className="formItem">
            <label htmlFor="creator">Creator (for public display; default: your username)</label>
            <input type="text" name="creator" 
              onBlur={setField(setCreator)}
              defaultValue={creator}/>
          </div>
        </div>
      </form>
    );
  }

  function CurrentFactsPanel() {
    function currentFact(item) {
      return(
        <tr key={crypto.randomUUID()}>
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
              <th>Year</th>
              <th>Description</th>
              <th>Image URL</th>
            </tr>
          </thead>
          <tbody>
            { events ? events.map(currentFact) : null }
          </tbody>
        </table>
        { events ? null : <Instructions /> }
      </section>
    );
  }

  function NewFactForm() {

    let [testCardDate, setTestCardDate] = useState(new Date());
    let [testCardInfo, setTestCardInfo] = useState("Description");
    let [testCardImg, setTestCardImg] = useState("https://picsum.photos/200.jpg");

    function newFact(event) {
      if (testCardDate && testCardInfo) {
        let newCard = new Card({
          date: testCardDate,
          info: testCardInfo,
          img: testCardImg
        });
        setEvents(prev => prev.addFact(newCard));
        console.debug("Added fact to timeline");
        updateTimeline();
      }
    }

    function CardPreview({ date, info, img }) {
      let year = date.getFullYear();
      return(
        <div className="card" data-when={year} data-noselect="noselect">
          <span className="date">{year}</span>
          <img alt={img} src={img} />
          <span className="info">{info}</span>
        </div>
      );
    }

    function setDate(event) {
      let date = new Date();
      date.setFullYear(Number(event.target.value));
      setTestCardDate(date);
    }

    function setInfo(event) {
      setTestCardInfo(event.target.value);
    }

    function setImg(event) {
      setTestCardImg(event.target.value);
    }

    return(
      <section id="new">
        <h2>Add an Event</h2>
        <form id="addEventForm">
          <div className="formInputBlock">
            <div className="formItem">
              <label htmlFor="date">Year</label>
              <input 
                type="number" 
                name="date" 
                onChange={setDate}
                defaultValue={testCardDate.getFullYear()} />
            </div>
            <div className="formItem">
              <label htmlFor="info">Description of event</label>
              <input 
                type="text" 
                name="info" 
                onChange={setInfo}
                defaultValue={testCardInfo} />
            </div>
            <div className="formItem">
              <label htmlFor="img">Complete URL of image (optional)</label>
              <input 
                type="url" 
                name="img" 
                onChange={setImg}
                defaultValue={testCardImg} />
            </div>
          </div>
          <section id="preview">
            <h3>Preview</h3>
            <CardPreview 
              date={testCardDate} 
              info={testCardInfo} 
              img={testCardImg} />
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
    updateTimeline();
    let action = timeline ? "Updated" : "Created";
    console.debug(`${action} timeline with title '${title}'`);

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
      setTitle("");
      setDescription("");
      setKeywords([""]);
      setCreator(currentUser.username);
      setEvents(new FactList());
      setTimeline(null);
    }
  }
 
 
  useEffect(() => {
    async function postTimeline(user, token, timeline) {
      console.debug(timeline.events);
      console.debug(timeline.json());
      let response = await fetch(`${User.SERVER}/timelines/create/`, {
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
      <MetadataPanel />
      <CurrentFactsPanel />
      <NewFactForm />
      <SaveButton />
      <ResetButton />
    </main>
  );
}

