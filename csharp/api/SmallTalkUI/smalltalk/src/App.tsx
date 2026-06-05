import './App.css'
import { useState, useEffect } from 'react';

const backendApi = "http://localhost:5009";

// TODO START here: assign random user id and only show that user's messages
let userId = Math.floor(Math.random() * 1000); // TODO assign (get from server)
let computerId = 0; // TODO get from server

// TODO make form send message
function MessageInputForm({ setMessages }) {
  async function postMessage(event) {
    event.preventDefault();

    const form = event.target;
    const formData = new FormData(form);
    event.target.reset();

    let response = await fetch(`${backendApi}/chat`, { 
      method: "POST", 
      headers: {
        "Content-Type": "application/json"
      },
      body: JSON.stringify({
        "senderId":     userId,
        "recipientId":  computerId,
        "time":         new Date().toISOString(),
        "text":         formData.get("text"),
      }),
    });

    console.log(response);

    await updateMessages(setMessages);
  }

  return(
    <form className="messageInputForm" onSubmit={postMessage}>
      <label htmlFor="text">Type your message:</label>
      <input name="text" />
      <button type="submit">Send</button>
    </form>
  );
}

class Message {
  constructor({ id, senderId, recipientId, time, text }) {
    this.id          = id;
    this.senderId    = senderId;
    this.recipientId = recipientId;
    this.time        = new Date(time);
    this.text        = text;
  }
}

function messageDisplay(message: Message) {
  console.log("messageDisplay");
  console.log(message);

  let messageType;
  if (message.recipientId == computerId) {
    messageType = "messageToComputer";
  } else if (message.senderId == computerId) {
    messageType = "messageFromComputer";
  } // else? TODO should only show messages from one sender

  // console.log(messageType);

  return (
    <div key={`msg${message.id}`} className={messageType}>
      <p className="chatText">{message.text}</p>
      <p className="chatTime">{chatTimeFormat(message.time)}</p>
    </div>
  );
}

function chatTimeFormat(timeString) {
  // TODO only show time if date is today
  return new Date(timeString).toLocaleTimeString();
}

const Conversation = ({ messages }) => {
  console.log(messages);
  return (
    <div className="conversation">
      <div className="messageScroll">
        {messages.map(messageDisplay)}
      </div>
    </div>
  );
}

async function updateMessages(setMessages): void {
  let response = await fetch(`${backendApi}/history`, {
    method: "POST",
    headers: {
      "Content-Type": "application/json"
    },
    body: JSON.stringify({
      "userId":     userId,
    }),
  });

  console.log(response);
  
  let json = await response.json();
  console.log(`$json: ${json}`);
  let messages = json.map(msg => new Message(msg));
  console.log(`New messages: ${messages}`);
  setMessages(prevMessages => messages);
}

function App() {
  const [messages, setMessages] = useState([]);
  useEffect(() => { updateMessages(setMessages) }, []);

  return (
    <>
      <h1>Small Talk</h1>
      <section>
        <div className="chatbox">
          <Conversation messages={messages} />
          <MessageInputForm setMessages={setMessages} />
        </div>
      </section>
    </>
  );
}

export default App
