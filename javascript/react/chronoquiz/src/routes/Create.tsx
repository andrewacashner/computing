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

  get keywordString() {
    return this.keywords.join("; ");
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

    return(
      <section id="currentTimeline">
        <h2>Current Timeline Facts</h2>
        <table className="timeline">
          <thead>
            <tr>
              <th>Year</th>
              <th>Description</th>
              <th>Image URL</th>
            </tr>
          </thead>
          <tbody>
            { timeline ? timeline?.events.map(currentFact) : null }
          </tbody>
        </table>
      </section>
    );
  }

  function NewFactForm() {

    let [testCardDate, setTestCardDate] = useState(new Date());
    let [testCardInfo, setTestCardInfo] = useState("Description");
    let [testCardImg, setTestCardImg] = useState("https://picsum.photos/200.jpg");

    function newFact(event) {
      event.preventDefault();
      if (event.target.date.value && event.target.info.value) {
        let newCard = new Card({
          date: testCardDate,
          info: testCardInfo,
          img: testCardImg
        });
        if (timeline) {
          setTimeline(prev => prev.addFact(newCard));
          console.debug("Added fact to timeline");
        }
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
        <h2>Add an Fact</h2>
        <form onSubmit={newFact}>
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
          <button type="submit">Add</button>
        </form>
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
        setSaveReady(false);
      } else {
        console.debug(`Problem creating timeline: Server status ${response.status}, ${response.statusText}`);
      }
    }
    if (saveReady && timeline) {
      console.debug("Ready to post timeline");
      console.debug(timeline.events);
      postTimeline(currentUser, userToken, timeline);
    } 
  }, [saveReady, timeline, currentUser, userToken]);


  return(
    <main>
      <h1>Create a Chronoquiz</h1>
      <Instructions />
      <form className="timelinePanel" onSubmit={createTimeline}>
        <MetadataPanel />
        <CurrentFactsPanel />
        <SubmitButton />
      </form>
      <NewFactForm />
    </main>
  );
}

