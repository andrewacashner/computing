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

import { default as CardDisplay } from "../components/Card";

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
    events  = new FactList() 
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

  get keywordString() {
    return this.keywords.join("; ");
  }

  addEvent(event: Card): Timeline {
    return new Timeline({
      ...this,
      events: this.events.appendClone(event)
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
  let [testCard, setTestCard] = useState(new Card({
    date: new Date(),
    info: "",
    img: ""
  }));

  function Instructions() {
    return(
      <p className="instructions">Your data will not be saved until you click Save.</p>
    );
  }

  function MetadataPanel() {
    function getDefault(field, fallback="") {
      return timeline ? timeline[field] : fallback;
    }

    let defaults = { 
      title: getDefault("title"),
      description: getDefault("description"),
      keywords: getDefault("keywordString"),
      creator: getDefault("creator", currentUser.username)
    }


   return(
     <div className="formInputBlock">
       <div className="formItem">
         <label htmlFor="title">Title</label>
         <input type="text" name="title" defaultValue={defaults.title} />
       </div>
       <div className="formItem">
         <label htmlFor="description">Description</label>
         <input type="text" name="description" defaultValue={defaults.description} />
       </div>
       <div className="formItem">
         <label htmlFor="keywords">Keywords (separated with semicolons)</label>
         <input type="text" name="keywords" defaultValue={defaults.keywords} />
       </div>
       <div className="formItem">
         <label htmlFor="creator">Creator (for public display; default: your username)</label>
         <input type="text" name="creator" defaultValue={defaults.creator}/>
       </div>
     </div>
    );
  }

  function CurrentEventsPanel() {
    function currentEvent(item) {
      return(
        <tr key={item.info} >
          <td>{item.date}</td>
          <td>{item.info}</td>
          <td>{item.img}</td>
        </tr>
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
            { timeline ? timeline.events.map(currentEvent) : null }
          </tbody>
        </table>
      </section>
    );
  }

  function NewEventForm() {
    function addEvent(event) {
      event.preventDefault();
      if (event.target.date.value && event.target.info.value) {
        let newCard = Card.newSafeCard({
          date: event.target.date.value,
          info: event.target.info.value,
          img: event.target.img.value
        });
        setTestCard(newCard);
        if (timeline) {
          setTimeline(prev => prev.addEvent(newCard));
          console.debug("Added event to timeline");
        }
      }
    }

    //    function preview(event) {
    //      let newCard = Card.newSafeCard({
    //        date: event.target.date.value,
    //        info: event.target.info.value,
    //        img: event.target.img.value
    //      });
    //      setTestCard(newCard);
    //    }

    let [testCardDate, setTestCardDate] = useState(new Date());
    let [testCardInfo, setTestCardInfo] = useState("");
    let [testCardImg, setTestCardImg] = useState("");

    function setDate(event) {
      let date = new Date();
      date.setFullYear(Number(event.target.value));
      console.debug(date);
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
        <form onSubmit={addEvent}>
          <div className="formInputBlock">
            <div className="formItem">
              <label htmlFor="date">Year</label>
              <input 
                type="number" 
                name="date" 
                onChange={setDate}
                defaultValue={testCardDate} />
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
          <button type="submit">Add</button>
        </form>
        <section id="preview">
          <h3>Preview</h3>
          <CardDisplay>{new Card({
            isClue: false,
            date: testCardDate,
            info: testCardInfo,
            img: testCardImg,
            color: "gray" // TODO 
          })}</CardDisplay>
        </section>
      </section>
    );
  }

  function SubmitButton() {
    return(
      <button type="submit">Save</button>
    );
  }
  
  function createTimeline(event) {
    event.preventDefault();
    if (event.target.title.value) {
      if (timeline) { console.debug(timeline.events); }
      let timelineInput = {
        title: event.target.title.value,
        description: event.target.description.value,
        keywords: Timeline.parseKeywords(event.target.keywords.value),
        creator: event.target.creator.value || currentUser.username,
        events: timeline ? timeline.events : new FactList()
      }
      
      let action = timeline ? "Updated" : "Created";
      console.debug(`${action} new timeline with title ${event.target.title.value}`);

      console.debug(timelineInput.events);
      setTimeline(new Timeline(timelineInput));
      setSaveReady(true);
    } else {
      setSaveReady(false);
    }
  }

 
  useEffect(() => {
    async function postTimeline(user, token, timeline) {
      let response = await fetch(`${User.SERVER}/timelines/create/`, {
        method: "POST",
        headers: new Headers({
          "Content-Type": "application/json",
          "Accept": "application/json",
          "Authorization": `Token ${token}`
        }),
        body: JSON.stringify(timeline)
      });
      if (response.ok) {
        let json = await response.json();
        console.debug(json);
        setSaveReady(false);
      } else {
        console.debug(`Problem creating timeline: Server status ${response.status}, ${response.statusText}`);
      }
    }
    if (saveReady && timeline) {
      postTimeline(currentUser, userToken, timeline);
    } 
  }, [saveReady, timeline, currentUser, userToken]);


  return(
    <main>
      <h1>Create a Chronoquiz</h1>
      <Instructions />
      <form className="timelinePanel" onSubmit={createTimeline}>
        <MetadataPanel />
        <CurrentEventsPanel />
        <SubmitButton />
      </form>
      <NewEventForm />
    </main>
  );
}

