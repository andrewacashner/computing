import './App.css'

const backendApi = "http://localhost:5009/chat";

function postMessage() {
  console.log("POST");
}

let senderId = 1; // TODO assign (get from server)
let recipientId = 0; // TODO get from server

// TODO make form send message
function MessageInputForm() {
  async function handleSubmit(event) {
    event.preventDefault();

    const form = event.target;
    const formData = new FormData(form);

    response = await fetch(backendApi, { 
      method: "POST", 
      header: {
        "Content-Type": "application/json"
      },
      body: {
        "senderId": senderId,
        "recipientId": recipientId,
        "time": Date.now(),
        "text": formData.text,
      }
    });
  }

  return(
    <form>
      <label htmlFor="text">Type your message:</label>
      <input name="text" />
      <button type="submit">Send</button>
    </form>
  );
}

function App() {
  return (
    <>
      <h1>Small Talk</h1>
      <section>
        <div className="chatbox">
          <MessageInputForm />
        </div>
      </section>
    </>
  );
}

export default App
