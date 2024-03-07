
export default function RestartButton() {
  function restart() {
    window.location.reload();
  }

  return(
    <button type="button" id="restart" onClick={restart}>Choose Another Timeline</button>
  );
}
