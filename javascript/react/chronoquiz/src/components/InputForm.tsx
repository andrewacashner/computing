export default function InputForm({ updateFn }) {

  function userUploadUrl(files: FileList): string {
    return URL.createObjectURL(files[0]);
  }

  function getInputUrl(source: string, files: FileList): string {
    let url = "";
    if (source) {
      if (files.length > 0) {
        url = userUploadUrl(files);
      } else {
        url = `input/${source}.json`;
      }
    }
    return url;
  }

  // TODO START
  function loadTimeline(url: string): Timeline {
    let timeline = new Timeline();
    return timeline;
  }

  function loadAndPlay(event: React.FormEvent<HTMLFormElement>): void {
    let files = event.currentTarget.fileInput.files as FileList;
    let source = event.currentTarget.source.value as string;

    try {
      let url = getInputUrl(source, files);
      if (url) {
        console.log(`Loading file ${url}`);
        let timeline = loadTimeline(url);
        updateFn(timeline);
      } else {
        throw new Error("Invalid input, cannot play game.");
      }
    } catch(e) {
      console.error(e);
    }
  }

  return(
    <form id="inputForm" onSubmit={loadAndPlay}>
      <label htmlFor="source">Choose a timeline:</label>
      <select name="source" id="source" required defaultValue="music">
        <option value="music">Music</option>
        <option value="wars">Wars</option>
        <option value="upload">Upload a custom timeline...</option>
      </select>
      <div id="file" className="hide">
        <label htmlFor="fileInput">Upload JSON timeline file (<a href="about.html">?</a>)</label>
        <input id="fileInput" type="file" accept=".json" />
      </div>
      <button type="submit" id="playbutton">Play!</button>
    </form>
  );
}

