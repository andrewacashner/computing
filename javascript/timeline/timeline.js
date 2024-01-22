/**
 * @filedescription Timeline Game
 * @author Andrew A. Cashner
 * @copyright Copyright © 2024 Andrew A. Cashner
 * @version 0.0.1
 */

"use strict";

/**
 * We use this class to store information on the historical events used as
 * clues and inserted into the timeline, and to generate the HTML node for a
 * card (div.card). The HTML differs for clues vs. answers (clue shows "CLUE" as
 * its date and is a draggable object; answer shows the real date and is not
 * draggable). 
 *
 * @constructor
 * @param {number} date - Year of event 
 *      (NB - We cannot process more complex dates, just years)
 * @param {string} info - Brief description of event 
 */
class Card {

  /** @type {number} */
  date;

  /** @type {string} */
  info;

  constructor(date, info, img) {
    this.id = crypto.randomUUID();
    this.date = date;
    this.info = info;
    this.img = img;
  }

  /**
   * Create HTML div.card node
   * @param {string} mode - "answer" or other ("question")
   *
   */
  toHtml(mode) {
    let card = document.createElement("div");
    card.className = "card";
    card.id = this.id;
    card.setAttribute("data-when", this.date);

    if (mode !== "answer") {
      makeDraggable(card);
    }

    let dateNode = document.createElement("span");
    dateNode.className = "date";

    if (mode === "answer") {
      dateNode.textContent = this.date;
    } else {
      dateNode.textContent = "Clue";
    }

    card.appendChild(dateNode);

    let infoNode = document.createElement("span");
    infoNode.className = "info";
    infoNode.textContent = this.info;

    if (this.img) {
      let imageNode = document.createElement("img");
      imageNode.src = this.img;
      card.appendChild(imageNode);
    }

    card.appendChild(infoNode);
    


    return card;
  }
}

/** 
 * Set node as a drop target.
 * @param {Element} cardNode - a Card DOM Element (div.card)
 */
function makeDropTarget(el) {
  el.setAttribute("ondrop", "dropHandler(event)");
  el.setAttribute("ondragover", "dragoverHandler(event)");
}

/** 
 * Set card node as a draggable object, not a drop target.
 * @param {Element} cardNode - a Card DOM Element (div.card)
 */
function makeDraggable(cardNode) {
  cardNode.setAttribute("draggable", "true");
  cardNode.setAttribute("ondragstart", "dragstartHandler(event)");
}

/**
 * Return a random integer up to the given max.
 * @param {number} max 
 * @returns {number}
 */
function randomInt(max) {
  return Math.floor(Math.random() * max);
}

/**
 * Return a shuffled copy of a given array, using the Fisher-Yates/Knuth
 * shuffle (`https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle`)
 * @param {Array} array
 * @returns {Array} New, shuffled array
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

/**
 * This class holds a list of facts (date + info) for the timeline, in its
 * facts member.
 *
 * @constructor
 * @param {Array} factArray - List of objects with date and info fields
 */
class FactList {

  /** @type {Array} */
  facts;

  /**
   * Given information, construct an array of Card instances, shuffle it, and
   * store it in this class's facts member.
   */
  constructor(factArray) {
    let clueList = [];
    for (let fact of factArray) {
      let newClue = new Card(fact.date, fact.info, fact.img);
      clueList.push(newClue);
    }
    this.facts = newShuffledArray(clueList);
  }

  /**
   * Randomly remove one fact from the list and return that fact.
   * @returns {Card} Card instance
   */
  extractEvent() {
    let i = randomInt(this.facts.length);
    let fact = this.facts[i];

    this.facts.splice(i, 1);
    return fact;
  }

  /**
   * Are there no facts left?
   * @returns {bool}
   */
  isEmpty() {
    return this.facts.length === 0;
  }
}

/**
 * Procedure: Update the page with a "Game Over" message including the final
 * score.
 * @param {number} score
 */
function gameOver(score) {
  console.log("Game over");
  let clueBay = document.querySelector("div.clue");
  let pointWord = "point";
  if (score !== 1) {
    pointWord = "points";
  }

  clueBay.innerHTML = 
    `<div class="gameover">
          <p>Game over!</p>
          <p>Final score: ${score} ${pointWord}</p>
        </div>`;
}

/**
 * Procedure: Given a parent node, extract a clue, create a clue card, and
 * append it as a child. (Removes a clue from the given FactList.)
 * @param {Element} clueBay - div.clue to hold div.card object
 * @param {FactList} clues - Source of clue card data
 */
function appendNextClue(clueBay, clues) {
  let clue = clues.extractEvent();
  let clueNode = clue.toHtml("clue");
  clueBay.appendChild(clueNode);
}

/**
 * Procedure: Draw the next clue and update the page to display it; if no
 * clues left, do gameOver. Use the div.clue element as the space for the
 * final message. Remove one card from card stubs showing in the deck.
 */
function drawNextClue(state) {
  if (state.clues.isEmpty()) {
    gameOver(state.score);
  } else {
    console.log(`Clues remaining: ${state.clues.facts.length}`);
    let clueBay = document.querySelector("div.clue");
    appendNextClue(clueBay, state.clues);
    
    // Remove one card from stubs showing remaining cards
    clueBay.removeChild(clueBay.firstChild);
  }
}

/**
 * Procedure: Put a starting card with today's date in the timeline.
 */
function initializeTimeline() {
  function firstEvent() {
    let thisYear = new Date().getFullYear();
    let now = new Card(thisYear, "Now");
    let nowNode = now.toHtml("answer");
    setColor(nowNode, VIOLET);
    return nowNode;
  }

  let now = firstEvent();
  let timelineBay = document.querySelector("div.timeline");
  makeDropTarget(timelineBay);
  removeChildren(timelineBay);
  timelineBay.appendChild(now);
}

/**
 * Procedure: Update the page with the given score.
 * @param {number} score
 */
function displayScore(score) {
  let scoreNode = document.querySelector("span.score");
  scoreNode.textContent = score;
}

/**
 * Procedure: When a card is dragged, transfer its date and the distance from
 * the click point to the right edge (used to calculate end position on
 * timeline).
 * @param {Event} event
 */
function dragstartHandler(event) {
  event.dataTransfer.setData("date", event.target.dataset.when);
  event.dataTransfer.setData("id", event.target.id);
  
  let boundingBox = event.target.getBoundingClientRect();
  event.dataTransfer.setData("toRightEdge", boundingBox.right - event.clientX);

  event.dataTransfer.effectAllowed = "move";
}

/**
 * Procedure: Allow to move by dragging.
 * @param {Event} event
 */
function dragoverHandler(event) {
  event.preventDefault();
  event.dataTransfer.effectAllowed = "move";
}

/**
 * Reset a clue card to be an answer card: Show the date.
 * @param {Element} card node
 * @returns {Element} card node
 */
function clueToAnswer(clue) {
  let clueDateText = clue.querySelector("span.date");
  clueDateText.textContent = clue.dataset.when;

  return clue;
}


/**
 * Given an event (e.g., from a drop), start from its coordinates and search
 * to the right until a card element is found. We measure the card
 * relative to its right edge: the guess is the next card whose right edge is
 * to the right edge of the clue when dropped. Return the answer card or null.
 * @param {Event} event 
 * @returns {Element} Card element or null
 */
function findFirstCardToRight(event) {

  /** Return a card element, if found at given coordinates; or null. */
  function cardAtCoord(x, y) {
    let card = null;
    let el = document.elementFromPoint(x, y);
    if (el.className === "card") {
      card = el;
    }
    return card;
  }

  let x = event.clientX;
  let y = event.clientY;
  // console.log(`Card dropped with pointer at (${x}, ${y})`);

  // Right edge of clue must be left of right edge of after-answer
  // Right edge of clue must be right of right edge of before-answer
  let rightDistance = event.dataTransfer.getData("toRightEdge");

  // Need to include margin on right edge
  let cardStyle = window.getComputedStyle(document.querySelector("div.card"));
  let cardMargin = cardStyle.getPropertyValue("margin-right");
  let rightEdge = x + parseFloat(rightDistance) + parseFloat(cardMargin);
  // console.log(`Search relative to right edge at ${rightEdge}`);

  let card;
  let testCard = cardAtCoord(rightEdge, y);

  if (testCard) {
    card = testCard;
  } else {
    console.log("Looking for nearest card to timeline drop point");
    let max = document.documentElement.clientWidth;
    
    for (let testX = x; testX < max; ++testX) {
      card = cardAtCoord(testX, y);
      if (card) {
        break;
      }
    }
  } 
  return card;
}

/** 
 * Procedure: Wait the given time.
 * @param {number} ms - milliseconds
 */
function sleep(ms = 0) {
  return new Promise(function (resolve) {
    setTimeout(resolve, ms)
  });
}

/**
 * Procedure: Quickly toggle the given element's alert state on and off twice.
 * @param {Element} el - DOM Element
 */
async function flashAlert(el) {
  for (let i = 0; i < 2; ++i) {
    el.setAttribute("data-alert", "true");
    await sleep(100);
    el.removeAttribute("data-alert");
    await sleep(100);
  }
}

/**
 * Procedure: When the user drops a card onto a timeline card, 
 * find the closest card, test if the date on the clue is between that card
 * and its previous neighbor (if there is one); if so insert the card and
 * increment the score; if not, do not insert the card and decrement the
 * score.
 * @param {Event} event
 */
function dropHandler(event) {

  /**
   * Is the given clue between a given answer card and the one before it?
   * @param {Element} clue - card node
   * @param {Element} guess - card node where clue was dropped
   * @param {Element} preGuess - previousSibling to guess (could be null)
   * @returns {bool}
   */
  function isClueBetweenDates(clue, guess, preGuess) {
    let clueDate = clue.dataset.when;
    let guessDate = guess.dataset.when;

    let isBeforeGuess = clueDate <= guessDate;

    let noPreGuess = !preGuess;
    let isAfterPreGuess = preGuess && (clueDate >= preGuess.dataset.when);
    let isAfterAnyPreGuess = noPreGuess || isAfterPreGuess;

    return isBeforeGuess && isAfterAnyPreGuess;
  }

  event.preventDefault();
  let date = event.dataTransfer.getData("date");
  console.log(`Dropping card with date ${date}`);

  // Show date on clue card 
  let clue = document.getElementById(event.dataTransfer.getData("id"));
  let clueDate = clue.dataset.when;

  // Find nearest answer (first card found to right of click) to compare
  let guess = findFirstCardToRight(event);

  if (guess) {
    let beforeGuess = guess.previousElementSibling;

    if (isClueBetweenDates(clue, guess, beforeGuess)) {
      console.log("Correct: ++Score");

      // Make clue card into a timeline card and insert
      let answer = clueToAnswer(clue);
      guess.insertAdjacentElement("beforebegin", answer);

      let cards = document.querySelectorAll("div.timeline div.card");
      setCardColors(cards, SPECTRUM);

      // TODO play sound, alert
      ++window.gameState.score;
      displayScore(window.gameState.score);

      drawNextClue(window.gameState);
    } else {
      // TODO play sound, alert
      flashAlert(clue);
      console.log("Incorrect, --Score");
      if (window.gameState.score > 0) {
        --window.gameState.score;
      }
      displayScore(window.gameState.score);
    }
  } else {
    console.log("No card found at drop location");
  }
}

/**
 * Colors: This class holds the information for one color: red, green, blue
 * values plus a percentage of white to mix in.
 * @constructor
 * @param {number} r - red, integer 0 <= n < 256
 * @param {number} g - green, integer 0 <= n < 256
 * @param {number} b - blue, integer 0 <= n < 256
 * @param {number} percentWhite - integer percentage of white to mix in 
 *      (50 = * 50%)
 */
class RgbColorMix {
  red;
  green;
  blue;
  percentWhite;

  constructor(r, g, b, w) {
    this.red = r;
    this.green = g;
    this.blue = b;
    this.percentWhite = w; // as decimal, 0.5 not 50%
  }

  /**
   * Create CSS color (color-mix with rgb color)
   * @returns {string} CSS color-mix expression
   */
  toCss() {
    let rgb = `rgb(${this.red}, ${this.green}, ${this.blue})`;
    return `color-mix(in srgb, ${rgb}, ${this.percentWhite}% white)`;
  }
}

/**
 * List of all colors available in range.
 * For each of red, blue, and green, iterate through values of primary with
 * constant secondary and white values (tertiary color is zero).
 *
 * @param {number} max - Highest color value possible for each 
 *      (red, green, blue)
 * @param {number} min - Used for secondary color, 
 *      fixed value mixed in to each primary
 * @param {number} white - Percent white to mix in, fixed for all
 * @returns {Array} array of RgbColorMix instances
 */
function colorSpectrum(max, min, white) {
  let reds = [];
  let blues = [];
  let greens = [];

  // Increase red against others to go red -> orange
  for (let i = 0; i < max; ++i) {
    reds.push   ([max, i, min, white]);
  }
  // *Decrease* green and blue against others to continue in spectrum order
  for (let i = max - 1; i >= 0; --i) {
    greens.push ([i, max, min, white]);
    blues.push  ([min, i, max, white]);
  }
  let perms = [...reds, ...greens, ...blues];
  let colors = perms.map((p) => new RgbColorMix(...p));
  return colors;
}

/**
 * Available spectrum of colors up to max, with given white proportion,
 * red -> violet order.
 */
const SPECTRUM = colorSpectrum(256, 0, 50);

/** Right-most, final color of spectrum. */
const VIOLET = SPECTRUM[SPECTRUM.length - 1];

/** Left-most color. */
const RED = SPECTRUM[0];

/**
 * Procedure: Set an element's inline style to the given RgbColorMix.
 * @param {Element} el - DOM element
 * @param {RgbColorMix} color
 */
function setColor(el, color) {
  el.style.backgroundColor = color.toCss();
}

/** Procedure: Set the colors of a set of cards (timeline) to equidistant
 * points on the color spectrum, ROYGBIV order. The rightmost card was already
 * set during initialization, so we skip it.
 *
 * @param {NodeList} cards - Node list of div.card DOM elements
 * @param {Array} spectrum - Array of RgbColorMix instances
 */
function setCardColors(cards, spectrum) {
  let cardMax = cards.length;
  let colorMax = spectrum.length;
  let interval = Math.floor(colorMax / cardMax);
 
  // We already set the rightmost card to be violet in page setup
  // Leftmost card is always red
  if (cardMax > 1) {
    setColor(cards[0], RED);

    if (cardMax > 2) {
      let thisCard = cardMax - 2;
      let thisColor = colorMax - 1 - interval;
      while (thisCard > 0 && thisColor > 0) {
        setColor(cards[thisCard], spectrum[thisColor]);
        --thisCard;
        thisColor = thisColor - interval;
      }
    }
  }
}

function removeChildren(node) {
  let child = node.lastChild;
  while (child) {
    node.removeChild(child);
    child = node.lastChild;
  }
}

/**
 * Procedure: Fill the clue deck with stubs for the remaining clues.
 * @param {number} n - Number of clues
 */
function fillDeck(n) {
  console.log(`Filling deck with ${n} cards`);
  let deck = document.querySelector("div.clue");
  removeChildren(deck);

  for (let i = 0; i < n - 1; ++i) {
    let card = document.createElement("div");
    card.className = "cardStub";
    deck.appendChild(card);
  }
}

function restart() {
  console.log("Restart");
  let input = document.getElementById("fileInput");
  let form = document.getElementById("inputForm");
  form.reset();
  location.reload();
}

function userUploadUrl(input) {
  let infile = input.files[0];
  let url = URL.createObjectURL(infile);
  return url;
}

/**
 * Load a JSON timeline from a given URL.
 * @param {string} URL
 * @returns {Array} Array of timeline facts
 */
async function loadTimeline(url) {
  const response = await fetch(url);
  const data = await response.json().catch((err) => {
    console.error(err);
    return [];
  });
  let timeline;
  if (isInputValid(data)) {
    timeline = data;
  }
  return timeline;
}

function isInputValid(timeline) {
  let isArray = Array.isArray(timeline);
  let isNotEmpty = timeline.length > 0;
  let hasProperFields = timeline.every(
    function (fact) {
      return (("date" in fact) && ("info" in fact));
    });
  return (isArray && isNotEmpty && hasProperFields);
}

function playGame(url) {
  let selector = document.getElementById("inputForm");
  selector.className = "hide";

  let uploadButton = document.getElementById("file");
  uploadButton.className = "hide";

  loadTimeline(url).then( function(timeline) {
    if (timeline) {
      let clues = new FactList(timeline);

      window.gameState = {
        clues: clues,
        score: 0
      }

      initializeTimeline();
      fillDeck(clues.facts.length);

      let clueBay = document.querySelector("div.clue");
      appendNextClue(clueBay, clues);
    } else {
      console.log(`Invalid timeline input from ${url}`);
      alert("Invalid timeline input");
//      restart();
    }
  });
}

/**
 * On page load, set up the timeline, initialize game state, and draw and
 * display the first clue.
 */
document.addEventListener("DOMContentLoaded", function (event) {
  let source = document.getElementById("source");
  source.addEventListener("change", 
    function() {
      let fileSelector = document.getElementById("file");
      if (source.value === "upload") {
        fileSelector.className = "show";
      } else {
        fileSelector.className = "hide";
      }
    }
  );

  let playButton = document.getElementById("playbutton");
  playButton.addEventListener("click", function () {
    let url;
    let input = document.getElementById("fileInput");
    let select = document.getElementById("source");
    if (select.value) {
      if (input.files.length > 0) {
        url = userUploadUrl(input);
      } else {
        let choice = select.value;
        url = `input/${choice}.json`;
      }
      console.log(`Loading file ${url}`);
      playGame(url);
    } else {
      console.log("No input source selected");
    }
  });
});

// TESTING
/** 
 * Create spectrum showing colors at each index
 * @param {Array} spectrum - array of RgbColorMix instances
 * @returns {Element} Div DOM element containing spans for each color
 */
function showColorSpectrum(spectrum) {
  let tree = document.createElement("div");
  for (let i = 0; i < spectrum.length; ++i) {
    let span = document.createElement("span");
    span.textContent = `${i}|`;
    setColor(span, spectrum[i]);
    tree.appendChild(span);
  }
  return tree;
}
