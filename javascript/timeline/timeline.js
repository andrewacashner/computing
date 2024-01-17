/* Timeline game
 * Andrew A. Cashner
 * 2024/01/16
 *
 * TODO
 * - allow user to supply timeline data (JSON or YAML, or custom interface)
 * - refactor code, separate functions
 */
const TIMELINE = [
    {   date: 1648,
        info: "End of the Thirty Years’ War."
    },
    {   date: 1865,
        info: "US Civil War ends with Lee’s surrender at Appomattox."
    },
    {   date: 1918,
        info: "World War I ends in an armistice."
    },
    {   date: 1939,
        info: "End of Spanish Civil War."
    },
    {   date: 1945,
        info: "World War II ends with the unconditional surrender of Japan."
    }
]

class Card {
    constructor(date, info) {
        this.date = date;
        this.info = info;
    }

    toHtml(mode) {
        let card = document.createElement("div");
        card.className = "card";
        card.setAttribute("data-when", this.date);

        if (mode === "answer") {
            card.setAttribute("ondrop", "dropHandler(event)");
            card.setAttribute("ondragover", "dragoverHandler(event)");
        } else {
            card.setAttribute("draggable", "true");
            card.setAttribute("ondragstart", "dragstartHandler(event)");
        }

        let dateNode = document.createElement("span");
        dateNode.className = "date";
        if (mode === "answer") {
            dateNode.textContent = this.date;
        } else {
            dateNode.textContent = "??";
        }

        let infoNode = document.createElement("span");
        infoNode.className = "info";
        infoNode.textContent = this.info;

        card.appendChild(dateNode);
        card.appendChild(infoNode);

        return card;
    }
}

function randomInt(max) {
    return Math.floor(Math.random() * max);
}


// Fisher-Yates/Knuth shuffle
// https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
function newShuffledArray(array) {

    function swapIndices(array, i, j) {
        [array[i], array[j]] = [array[j], array[i]];
        return array;
    }

    let newArray = [...array];
    for (let i = newArray.length - 1; i > 0; --i) {
        newArray = swapIndices(newArray, i, randomInt(i));
    }
    return newArray;
}

class FactList {
    constructor(factArray) {
        let clueList = [];
        for (let fact of factArray) {
            let newClue = new Card(fact.date, fact.info);
            clueList.push(newClue);
        }
        this.facts = newShuffledArray(clueList);
    }

    extractEvent() {
        let i = randomInt(this.facts.length);
        let fact = this.facts[i];

        this.facts.splice(i, 1);
        return fact;
    }

    isEmpty() {
        return this.facts.length === 0;
    }
}

function gameOver() {
    let clueBay = document.querySelector("div.clue");
    clueBay.innerHTML = `<div class="gameover"><p>Game over!</p><p>Final score: ${score} points</p></div>`;
}

function drawNextClue(factList) {
    if (factList.isEmpty()) {
        gameOver();
    } else {
        let clue = factList.extractEvent();
        let clueNode = clue.toHtml("clue");
        let clueBay = document.querySelector("div.clue");
        clueBay.appendChild(clueNode);
    }
}

function initializeTimeline() {
    function firstEvent() {
        let thisYear = new Date().getFullYear();
        let now = new Card(thisYear, "Now");
        let nowNode = now.toHtml("answer");
        return nowNode;
    }

    let now = firstEvent();
    let timelineBay = document.querySelector("div.timeline");
    timelineBay.appendChild(now);
}

function addUpdateScore(n) {
    score = score + n;
    let scoreNode = document.querySelector("span.score");
    scoreNode.textContent = score;
}

// TODO how to deal with globals
var clues = new FactList(TIMELINE);
var score = 0;

function setRestartButton() {
    let button = document.querySelector("button.restart");
    button.setAttribute("onclick", "location.reload()");
}

document.addEventListener("DOMContentLoaded", (event) => {
    setRestartButton();
    initializeTimeline();
    drawNextClue(clues);
});

function dragstartHandler(event) {
    event.dataTransfer.setData("date", event.target.dataset.when);
    event.dataTransfer.effectAllowed = "move";
}

function dragoverHandler(event) {
    event.preventDefault();
    event.dataTransfer.effectAllowed = "move";
}

function clueToAnswer(clue) {
    let clueDateText = clue.querySelector("span.date");
    clueDateText.textContent = clue.dataset.when;

    clue.removeAttribute("draggable");
    clue.removeAttribute("ondragstart");
    clue.setAttribute("ondrop", "dropHandler(event)");
    clue.setAttribute("ondragover", "dragoverHandler(event)");

    return clue;
}

function dropHandler(event) {
    event.preventDefault();
    let date = event.dataTransfer.getData("date");
    console.log(`Dropping card with date ${date}`);

    // Show date on clue card 
    let clue = document.querySelector(`div.card[data-when="${date}"]`);
    let clueDate = clue.dataset.when;

    let guess = event.target.closest("div.card");
    let beforeGuess = guess.previousElementSibling;

    if ((clueDate <= guess.dataset.when)
        && ((!beforeGuess) ||
            (beforeGuess && (clueDate >= beforeGuess.dataset.when)))) {
        console.log("Correct");
        let answer = clueToAnswer(clue);
        guess.insertAdjacentElement("beforebegin", answer);
        addUpdateScore(1);
        drawNextClue(clues);
    } else {
        addUpdateScore(-1);
    }
}

