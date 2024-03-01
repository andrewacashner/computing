export default function InputForm() {
  return(
      <form id="inputForm">
        <label htmlFor="source">Choose a timeline:</label>
        <select name="source" id="source" required>
          <option value="music" selected>Music</option>
          <option value="wars">Wars</option>
          <option value="upload">Upload a custom timeline...</option>
        </select>
        <div id="file" className="hide">
          <label htmlFor="fileInput">Upload JSON timeline file (<a href="about.html">?</a>)</label>
          <input id="fileInput" type="file" accept=".json" />
        </div>
        <button type="button" id="playbutton">Play!</button>
      </form>
      );
}

