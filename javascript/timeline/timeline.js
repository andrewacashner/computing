/* Timeline game
 * Andrew A. Cashner
 * 2024/01/16
 *
 * TODO
 * - randomly select clues from JSON (or YAML) input
 * - allow user to supply timeline data
 * - add scoring
 * - refactor code, separate functions
 */

function dragstartHandler(event) {
    event.dataTransfer.setData("date", event.target.dataset.when);
    event.dataTransfer.effectAllowed = "move";
}

function dragoverHandler(event) {
    event.preventDefault();
    event.dataTransfer.effectAllowed = "move";
}

function dropHandler(event) {
    event.preventDefault();
    let date = event.dataTransfer.getData("date");
    console.log(`Dropping card with date ${date}`);

    // Show date on clue card
    let clue = document.querySelector(`div.card[data-when="${date}"]`);
    let clueDate = clue.dataset.when;
    let clueDateText = clue.querySelector("span.date");
    clueDateText.textContent = clueDate;

    let guess = event.target.closest("div.card");
    let beforeGuess = guess.previousElementSibling;

    if (clueDate <= guess.dataset.when) {
        if ((!beforeGuess) ||
            (beforeGuess && (clueDate >= beforeGuess.dataset.when))) {
            console.log("Correct");
            clue.removeAttribute("draggable");
            clue.removeAttribute("ondragstart");
            clue.setAttribute("ondrop", "dropHandler(event)");
            clue.setAttribute("ondragover", "dragoverHandler(event)");
            guess.insertAdjacentElement("beforebegin", clue);
        }
    }
}

function checkTimeline() {
    let timeline = document.querySelectorAll("div.timeline div.card");
    let dateCheck = true;
    for (let i = 0; i < timeline.length - 1; ++i) {
        if (timeline[i].dataset.when > timeline[i + 1].dataset.when) {
            dateCheck = false;
            break;
        }
    }
    if (dateCheck) {
        console.log("Right"); 
        clue.setAttribute("data-status", "");
    } else {
        console.log("Wrong");
        clue.setAttribute("data-status", "wrong");
    }
    return dateCheck;
}

