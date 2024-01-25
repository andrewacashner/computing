/**
 * @filedescription Timeline Game
 * @author Andrew A. Cashner
 * @copyright Copyright © 2024 Andrew A. Cashner
 * @version 0.1.0
 *
 * See README.md and about.html.
 *
 * The game state consists of the score, a list of clues, and a list of
 * answers that is the timeline.
 * Given an input file in JSON format (either local or supplied by the user),
 * we construct a deck of clues as a FactList instance, which contains an
 * array of fact Card instances.
 * The clue deck shows the current clue and the stubs of the remaining clues.
 * We shuffle the clue deck once at the beginning and display the timeline
 * with the "Now" card.
 *
 * At each turn, we update the DOM to display the current clue (last card in
 * the clue deck).
 * The user selects the place to insert the clue by dragging the clue card
 * onto the timeline.
 * While they are dragging, we update the display to highlight where they can
 * drop the card.
 * When they drop the card, the dropHandler function evaluates their guess and
 * updates the game's state accordingly.
 *
 * If the guess is correct, we remove the current clue from the list of clues
 * and add it to the list of answers.
 * We sort the answers chronologically, then update the timeline display with
 * the new card included.
 * We increment and redisplay the score.
 * If at this point there are no more clues, we display a game over message
 * with the final score.
 * Otherwise we update the clue display to show the next clue and delete one
 * of the stubs.
 *
 * If the guess is incorrect we flash the color of the current guess card and
 * then decrement (only to zero) and redisplay the score.
 *
 * Whenever we display the timeline, we set the colors of the cards to be
 * equally spaced on a rainbow color spectrum, with the first card always red
 * and the last always violet.
 * (Actually we use colors mixed with white for a more muted effect.)
 *
 * We have avoided global state and instead pass the game state (a Game
 * instance) only to the functions that need it.
 * The game state only changes when a card is dropped.
 * The drop handler function is added only when we generate a DOM element
 * from a card (Card.toHtml()), so we pass the state to that function, which
 * includes the state in its call to dropHandler().
 *
 * In the documentation I preface the decription with "Procedure" for all
 * functions that mutate their arguments and are used only for side effects,
 * not their return value.
 */

"use strict";

/**
 * We use this class to store information on the historical events used as
 * clues and inserted into the timeline, and to generate the HTML node for a
 * card (div.card). The HTML differs for clues vs. answers (clue shows "CLUE"
 * as its date and is a draggable object; answer shows the real date and is
 * not draggable). 
 *
 * @constructor
 * @param {number} year - Four-digit Year of event 
 *      (NB - We currently use years only)
 * @param {string} info - Brief description of event 
 * @param {string} img - URL of image (on web, not local)
 */
class Card {

  /** @type {number} */
  date;

  /** @type {string} */
  info;

  /** Each card gets the given info and a random unique identifier. */
  constructor(year, info, img) {
    this.id = crypto.randomUUID();
    
    this.date = new Date();
    this.date.setFullYear(year);

    this.info = info;
    this.img = img;
  }
  
  // PRIVATE METHODS

  // Local private symbols
  
  /** Symbol used to select generating an answer card. */
  #answer = Symbol("answer");

  /** Symbol to select generating a clue card. */
  #clue = Symbol("clue");

  /** Is this symbol 'answer'?
   * @param{symbol} sym
   */
  #isAnswer(sym) {
    return sym === this.#answer;
  }

  /**
   * Procedure: Add the element to display the date to the HTML card we are
   * making. In the date field, just show "Clue" if this is a clue.
   * @param{element} card -- HTML DOM object for a div.card
   * @param{symbol} mode -- 'answer' or 'clue' 
   */
  #addHtmlDate(card, mode = this.#clue) {
    let dateNode = document.createElement("span");
    dateNode.className = "date";
    dateNode.textContent = this.#isAnswer(mode) ? this.dateToString() : "Clue";
    card.appendChild(dateNode);
  }

  /**
   * Procedure: Add the element to display the description information to the
   * HTML card we are making.  
   * @param{element} card -- HTML DOM object for a div.card
   */
  #addHtmlInfo(card) {
    let infoNode = document.createElement("span");
    infoNode.className = "info";
    infoNode.textContent = this.info;
    card.appendChild(infoNode);
  }
  
  /**
   * Procedure: If there is an img field, add the element for the image to the
   * HTML card we are making.
   * @param{element} card -- HTML DOM object for a div.card
   */
  #addHtmlImg(card) {
    if (this.img) {
      let imageNode = document.createElement("img");
      imageNode.src = this.img;
      card.appendChild(imageNode);
    }
  }
  
  /** 
   * Procedure: Set node as a drop target. 
   *
   * IMPORTANT: We pass the game state to the drop handler function so that the
   * game can update when the card is dropped.
   * @param {element} el - a Card DOM Element (div.card)
   */
  #makeDropTarget(el, state) {
    el.addEventListener("drop", (e) => dropHandler(state, event));
    el.addEventListener("dragover", dragoverHandler);
    el.addEventListener("dragleave", dragleaveHandler);
  }

  /** 
   * Procedure: Set card node as a draggable object, not a drop target.
   * @param {element} el - a Card DOM Element (div.card)
   */
  #makeDraggable(el) {
    el.setAttribute("draggable", "true");
    el.addEventListener("dragstart", dragstartHandler);
  }

  /**
   * Create HTML div.card node
   * @param {symbol} mode - 'answer' or 'clue'
   *
   */
  #toHtml(mode, state) {
    let card = document.createElement("div");
    card.className = "card";
    card.id = this.id;
    card.setAttribute("data-when", this.date.getFullYear());

    // CSS will use this to make it impossible to select card contents
    // accidentally
    card.setAttribute("data-noselect", "noselect");

    // Only clues can be dragged
    if (this.#isAnswer(mode)) {
      this.#makeDropTarget(card, state);
    } else {
      this.#makeDraggable(card);
    }

    this.#addHtmlDate(card, mode);
    this.#addHtmlImg(card);
    this.#addHtmlInfo(card);
    
    return card;
  }

  // PUBLIC METHODS
  /**
   * Return the year if positive or year BC if negative. (Deals with the year
   * only.) 
   *
   * Technically BC should be offset by one year but we told users to use
   * negative numbers as years BC.
   *
   * @returns{string} Formatted string for year, with BC if the year was
   *    negative
   */
  dateToString() { 
    let yearZero = new Date();
    yearZero.setFullYear(0);

    let displayYear = this.date.getFullYear();
    if (this.date < yearZero) {
      displayYear = `${-displayYear} bce`; 
    } 
    return displayYear;
  }

  /**
   * Generate a DOM clue card element.
   * @param{Game} state - Game state object
   * @returns{element} - div.clue DOM element
   */
  toHtmlClue(state) {
    return this.#toHtml(this.clue, state);
  }

  /**
   * Generate a DOM answer card element.
   * @param{Game} state - Game state object
   * @returns{element} - div.clue DOM element
   */
  toHtmlAnswer(state) {
    return this.#toHtml(this.#answer, state);
  }
}



/**
 * This class holds a list of facts (date + info) for the timeline, in its
 * facts member.
 *
 * @constructor
 * @param {array} factArray - List of objects with date and info fields
 */
class FactList {

  /** @type {array} */
  facts;

  /**
   * Given information, construct an array of Card instances, shuffle it, and
   * store it in this class's facts member.
   */
  constructor(factArray) {
    this.facts = [...factArray];
    this.#shuffle();
  }

  /**
   * Return a shuffled copy of a given array, using the Fisher-Yates/Knuth
   * shuffle (`https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle`)
   * @param {array} array
   * @returns {array} New, shuffled array
   */
  #newShuffledArray(array) {
    /**
     * Return a random integer up to the given max.
     * @param {number} max 
     * @returns {number}
     */
    function randomInt(max) {
      return Math.floor(Math.random() * max);
    } 

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
 
  #shuffle() {
    this.facts = this.#newShuffledArray(this.facts);
  } 

  /**
   * Randomly remove one fact from the list and return that fact.
   * @returns {Card} Card instance
   */
  extractEvent() {
    let fact = this.facts.pop();
    return fact;
  }

 

  addEvent(card) {
    this.facts.push(card);
    this.facts.sort(function (card1, card2) { 
      let [date1, date2] = [card1, card2].map((c) => c.date.getFullYear());
      return date1 - date2;
    });
  }

  count() {
    return this.facts.length;
  }

  /**
   * Are there no facts left?
   * @returns {bool}
   */
  isEmpty() {
    return this.count() === 0;
  }

  last() {
    return this.facts[this.facts.length - 1];
  }
}

/**
 * Procedure: Update the page with a "Game Over" message including the final
 * score.
 * @param {number} score
 */
function gameOver(score) {
  console.log("Game over");
  let deck = document.querySelector("div.clue");
  let pointWord = (score !== 1) ? "points" : "point";

  deck.innerHTML = 
    `<div class="gameover">
          <p>Game over!</p>
          <p>Final score: ${score} ${pointWord}</p>
        </div>`;
}

/**
 * Procedure: When a card is dragged, transfer its date and the distance from
 * the click point to the right edge (used to calculate end position on
 * timeline).
 * @param {event} event
 */
function dragstartHandler(event) {
  event.dataTransfer.setData("id", event.target.id);
  event.dataTransfer.effectAllowed = "move";
}

const CARD_LEFT_MARGIN = "var(--card-margin)";

function setCardLeftMargin(card, val) {
  card.style.marginLeft = val;
}

function insertTimelineGap(card) {
  setCardLeftMargin(card, `calc(5 * ${CARD_LEFT_MARGIN}`);
}

function removeTimelineGap(card) {
  setCardLeftMargin(card, CARD_LEFT_MARGIN);
}

function midpoint(left, right) {
  return (right - left) / 2 + left;
}

/** Return a card element, if found at given coordinates; or null. */
function cardAtCoord(x, y) {
  let card = null;
  let el = document.elementFromPoint(x, y);
  if (isCard(el)) {
    card = el;
  }
  return card;
}


/**
 * Procedure: Allow to move by dragging.
 * @param {event} event
 */
function dragoverHandler(event) {
  event.preventDefault();

//  console.log(`Dragging over me ${event.target.dataset.when}`);
//  console.log(`Current x = ${event.clientX}`);
//  console.log(`Target left edge = ${event.target.getBoundingClientRect().left}`);

  let targetBounds = event.target.getBoundingClientRect();
  let targetCenter = midpoint(targetBounds.left, targetBounds.right);

  if (event.clientX <= targetCenter) {
    console.log("In range!");
    let cardUnderDrag = cardAtCoord(event.clientX, event.clientY);
    if (cardUnderDrag) {
      insertTimelineGap(cardUnderDrag);
    }
  }
  event.dataTransfer.effectAllowed = "move";
}

function isCard(el) {
  return el.className === "card";
}

function dragleaveHandler(event) {
  console.log("Dragged card leaving target!");
  let el = event.target
  if (isCard(el)) {
    removeTimelineGap(el);
  }
}


/**
 * Given an event (e.g., from a drop), start from its coordinates and search
 * to the right until a card element is found. The card must be dropped to
 * left of the midpoint of the card.
 * TODO Is there a better way?
 * Return the answer card or null.
 * @param {event} event 
 * @returns {element} Card element or null
 */
function findFirstCardToRight(event) {

  let x = event.clientX;
  console.log(`Card dropped with pointer at (${x}, ${event.clientY})`);

  // Search along the timeline bar regardless of where the drop was vertically
  let timelineBar = document.querySelector("div.timelineBar hr");
  let y = timelineBar.getBoundingClientRect().top;

  let card;
  console.log("Looking for nearest card to timeline drop point");
  let max = document.documentElement.clientWidth; // TODO window instead?

  for (let testX = x; testX < max; ++testX) {
    card = cardAtCoord(testX, y);
    if (card) {
      let cardCoords = card.getBoundingClientRect();
      let cardCenter = midpoint(cardCoords.left, cardCoords.right);

      if (x <= cardCenter) {
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
 * @param {element} el - DOM Element
 */
async function flashAlert(el) {
  for (let i = 0; i < 2; ++i) {
    el.setAttribute("data-alert", "alert");
    await sleep(100);
    el.removeAttribute("data-alert");
    await sleep(100);
  }
}

function updateTimeline(state) {
  let currentTimeline = document.querySelector("div.timeline");
  currentTimeline.replaceWith(state.timelineToHtml());
}

/**
 * Procedure: Update the page with the given score.
 * @param {number} score
 */
function updateScore(state) {
  let scoreNode = document.querySelector("span.score");
  scoreNode.replaceWith(state.scoreToHtml());
}

function updateDisplay(state) {
  updateScore(state);
  updateTimeline(state);
}




function updateClues(state) {
  let clues = state.clues;
  let currentDeckNode = document.querySelector("div.clue");
  let newDeckNode = document.createElement("div");
  newDeckNode.className = "clue";

  fillDeck(newDeckNode, clues.count());

  if (clues.count() > 0) {
    let currentClue = clues.last();
    let clueNode = currentClue.toHtmlClue(state);
    newDeckNode.appendChild(clueNode);
  }

  currentDeckNode.replaceWith(newDeckNode);
}




/**
 * Procedure: When the user drops a card onto a timeline card, 
 * find the closest card, test if the date on the clue is between that card
 * and its previous neighbor (if there is one); if so insert the card and
 * increment the score; if not, do not insert the card and decrement the
 * score.
 * @param {event} event
 */
function dropHandler(state, event) {
  /**
   * Is the given clue between a given answer card and the one before it?
   * @param {element} clue - card node
   * @param {element} guess - card node where clue was dropped
   * @param {element} preGuess - previousSibling to guess (could be null)
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

  // Find nearest answer (first card found to right of click) to compare
  let clue = document.getElementById(event.dataTransfer.getData("id"));
  let guess = findFirstCardToRight(event);

  if (guess) {
    let beforeGuess = guess.previousElementSibling;

    if (isClueBetweenDates(clue, guess, beforeGuess)) {
      // TODO play sound
      console.log("Correct: ++Score");
      state.incrementScore();
      state.advanceState();
      updateDisplay(state);
    } else {
      // TODO play sound
      console.log("Incorrect, --Score");
      flashAlert(clue);

      state.decrementScore();
      updateDisplay(state);
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
 * @returns {array} array of RgbColorMix instances
 */
function colorSpectrum(max, min, white) {
  let reds = [];
  let blues = [];
  let greens = [];

  // Increase red against others to go red -> orange
  for (let i = 0; i < max; ++i) {
    reds.push([max, i, min, white]);
  }
  // *Decrease* green and blue against others to continue in spectrum order
  for (let i = max - 1; i >= 0; --i) {
    greens.push([i, max, min, white]);
    blues.push([min, i, max, white]);
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
 * @param {element} el - DOM element
 * @param {RgbColorMix} color
 */
function setColor(el, color) {
  el.style.backgroundColor = color.toCss();
}

/** Procedure: Set the colors of a set of cards (timeline) to equidistant
 * points on the color spectrum, ROYGBIV order. The rightmost card was already
 * set during initialization, so we skip it.
 *
 * @param {element} timeline - div.timeline DOM element with div.card children
 * @param {array} spectrum - Array of RgbColorMix instances
 */
function setCardColors(timeline, spectrum) {
  let cards = timeline.querySelectorAll("div.card");
  let cardMax = cards.length;
  let colorMax = spectrum.length;
  let interval = Math.floor(colorMax / cardMax);

  // Rightmost card is always violet
  setColor(cards[cardMax - 1], VIOLET);

  // Leftmost card is always red
  if (cardMax > 1) {
    setColor(cards[0], RED);
  } 

  // The rest are evenly spaced on a spectrum
  if (cardMax > 1) {
    let thisCard = cardMax - 2;
    let thisColor = colorMax - 1 - interval;
    while (thisCard > 0 && thisColor > 0) {
      setColor(cards[thisCard], spectrum[thisColor]);
      --thisCard;
      thisColor = thisColor - interval;
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
function fillDeck(deck, n) {
  console.log(`Filling deck with ${n} cards`);
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

function isInputValid(json) {
  let isArray = Array.isArray(json);
  let isNotEmpty = json.length > 0;
  let hasProperFields = json.every(
    function (fact) {
      return (("date" in fact) && ("info" in fact));
    });
  return (isArray && isNotEmpty && hasProperFields);
}

function cardArrayFromJson(json) {
  return json.map((d) => new Card(d.date, d.info, d.img));
}

/**
 * Load a JSON timeline from a given URL.
 * @param {string} URL
 * @returns {array} Array of timeline facts
 */
async function loadTimeline(url) {
  let response = await fetch(url);
  let data = await response.json().catch((err) => {
    console.error(err);
    return [];
  });

  let cards = [];
  if (isInputValid(data)) {
    cards = cardArrayFromJson(data);
  }
  return cards;
}



const NOW_IMAGE_URL = "https://images.pexels.com/photos/17139860/pexels-photo-17139860/free-photo-of-hourglass-with-sand.jpeg";

class Game {
  clues;
  timeline;
  score;

  constructor(clues, timeline, score) {
    this.clues = clues;
    this.timeline = timeline;
    this.score = score;
  }

  incrementScore() {
    ++this.score;
  }

  decrementScore() {
    this.score  = Math.max(0, this.score - 1);
  }
 
  moveCurrentClueToTimeline() {
    this.timeline.addEvent(this.clues.extractEvent());
  }

  advanceState() {
    if (this.clues.count() == 1) { // Last clue was just answered
      gameOver(this.score);
    } else {
      this.moveCurrentClueToTimeline();
      updateClues(this);
    }
  }

  scoreToHtml() {
    let scoreNode = document.createElement("span");
    scoreNode.className = "score";
    scoreNode.textContent = this.score;
    return scoreNode;
  }

  timelineToHtml() {
    let timelineNode = document.createElement("div");
    timelineNode.className = "timeline";

    this.timeline.facts.map((fact) =>  {
      timelineNode.appendChild(fact.toHtmlAnswer(this));
    });

    setCardColors(timelineNode, SPECTRUM);
    return timelineNode;
  }
}

function playGame(url) {
  let selector = document.getElementById("inputForm");
  selector.className = "hide";

  let uploadButton = document.getElementById("file");
  uploadButton.className = "hide";

  let scoreDisplay = document.getElementById("score");
  scoreDisplay.className = "show";

  loadTimeline(url).then( function(timelineData) {
    if (timelineData) {
      let clues = new FactList(timelineData);

      let now = new Card(new Date().getFullYear(), "Now", NOW_IMAGE_URL);
      let timeline = new FactList([now]);

      let state = new Game(clues, timeline, 0);

      updateClues(state);
      updateDisplay(state);
    } else {
      console.log(`Invalid timeline input from ${url}`);
      alert("Invalid timeline input");
      restart();
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
 * @param {array} spectrum - array of RgbColorMix instances
 * @returns {element} Div DOM element containing spans for each color
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
