/**
 * Timeline Game
 * Andrew A. Cashner
 * 2024/01/16
 * 
 * In this game, players build a chronological timeline of historical events.
 * Given a clue with no date, the user drags the clue onto the timeline where
 * they think it belongs.
 * If they are correct, the clue is inserted in the timeline and they get a
 * point.
 * If they are incorrect, the clue is not inserted so they can guess again,
 * but they lose a point.
 * When all clues are done, the final score is shown.
 *
 * TODO
 * - allow user to supply timeline data (JSON or YAML, or custom interface)
 * - animation/color when user gets answer right or wrong
 * - documentation
 */

"use strict";

/**
 * Timeline data input
 *
 * TODO give user option to upload (or create) this data
 * (better to do that on a separate landing page)
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

/**
 * We use this class to store information on the historical events used as
 * clues and inserted into the timeline, and to generate the HTML node for a
 * card (div.card). The HTML differs for clues vs. answers (clue shows "??" as
 * its date and is a draggable object; answer shows the real date and is a
 * drop target).
 *
 * @constructor
 * @param {number} date - Year of event 
 *      (NB - We cannot process more complex dates, just years)
 * @param {string} info - Brief description of event 
 */
class Card {
    date;
    info;

    constructor(date, info) {
        this.date = date;
        this.info = info;
    }
    
    /**
     * Create HTML div.card node
     * @param {string} mode - "answer" or other ("question")
     *
     */
    toHtml(mode) {
        let card = document.createElement("div");
        card.className = "card";
        card.setAttribute("data-when", this.date);

        if (mode === "answer") {
            makeDropTarget(card);
        } else {
            makeDraggable(card);
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

/** 
 * Set card node as a drop target, not a draggable object.
 * @param {Element} cardNode - a Card DOM Element (div.card)
 */
function makeDropTarget(cardNode) {
    cardNode.removeAttribute("draggable");
    cardNode.removeAttribute("ondragstart");
    cardNode.setAttribute("ondrop", "dropHandler(event)");
    cardNode.setAttribute("ondragover", "dragoverHandler(event)");
}

/** 
 * Set card node as a draggable object, not a drop target.
 * @param {Element} cardNode - a Card DOM Element (div.card)
 */
function makeDraggable(cardNode) {
    cardNode.removeAttribute("ondrop");
    cardNode.removeAttribute("ondragover");
    cardNode.setAttribute("draggable", "true");
    cardNode.setAttribute("ondragstart", "dragstartHandler(event)");
}

/**
 * Return a random integer up to the given max.
 * @param {number} max
 */
function randomInt(max) {
    return Math.floor(Math.random() * max);
}

/**
 * Return a shuffled copy of a given array, using the Fisher-Yates/Knuth
 * shuffle (`https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle`)
 * @param {Array} array
 */
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

function gameOver(score) {
    let clueBay = document.querySelector("div.clue");
    clueBay.innerHTML = 
        `<div class="gameover">
          <p>Game over!</p>
          <p>Final score: ${score} points</p>
        </div>`;
}

function drawNextClue(state) {
    if (state.clues.isEmpty()) {
        gameOver(state.score);
    } else {
        let clue = state.clues.extractEvent();
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

function displayScore(score) {
    let scoreNode = document.querySelector("span.score");
    scoreNode.textContent = score;
}

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
    makeDropTarget(clue);

    return clue;
}

function isClueBetweenDates(clue, guess, preGuess) {
    let clueDate = clue.dataset.when;
    let guessDate = guess.dataset.when;
    
    let isBeforeGuess = clueDate <= guessDate;

    let noPreGuess = !preGuess;
    let isAfterPreGuess = preGuess && (clueDate >= preGuess.dataset.when);
    let isAfterAnyPreGuess = noPreGuess || isAfterPreGuess;

    return isBeforeGuess && isAfterAnyPreGuess;
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

    if (isClueBetweenDates(clue, guess, beforeGuess)) {
        console.log("Correct: ++Score");
        let answer = clueToAnswer(clue);
        guess.insertAdjacentElement("beforebegin", answer);

        ++window.gameState.score;
        displayScore(window.gameState.score);

        drawNextClue(window.gameState);
    } else {
        console.log("Incorrect: --Score");
        --window.gameState.score;
        displayScore(window.gameState.score);
    }
}

document.addEventListener("DOMContentLoaded", (event) => {
    let timeline = TIMELINE;

    window.gameState = {
        clues: new FactList(timeline),
        score: 0
    }

    initializeTimeline();
    drawNextClue(window.gameState);
});


