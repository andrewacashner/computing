// Drag and drop grocery items and sum prices
// Andrew Cashner, 2024/01/11
"use strict";

const inventory = [
    {
        "id": "potato",
        "label": "Potato",
        "price": 0.50,
        "img": "🥔"
    },
    {
        "id": "carrot",
        "label": "Carrot",
        "price": 0.25,
        "img" : "🥕"
    },
    { 
        "id": "eggplant",
        "price": 0.75,
        "label": "Eggplant",
        "img": "🍆"
    },
    {
        "id": "avocado",
        "price": 1.25,
        "label": "Avocado",
        "img": "🥑"
    },
    {
        "id": "chile",
        "price": 0.30,
        "label": "Chile Pepper",
        "img": "🌶️"
    },
    {
        "id": "bellPepper",
        "price": 0.40,
        "label": "Bell Pepper",
        "img": "🫑"
    },
    {
        "id": "onion",
        "price": 0.10,
        "label": "Onion",
        "img": "🧅"
    },
    {
        "id": "beans",
        "price": 0.05,
        "label": "Kidney Beans",
        "img": "🫘"
    }
]

function formatPrice(n) {
    return "$" + parseFloat(n).toFixed(2);
}

function newBin(product) {

    function newHead(product) {
        let head = document.createElement("h1");
        head.textContent = product.label;

        let priceLabel = document.createElement("span");
        priceLabel.textContent = formatPrice(product.price);
        head.appendChild(priceLabel);

        console.log("Created bin label");
        console.log(head);

        return head;
    }

    console.log(`Setting up bin ${product.id}`);
    let bin = document.createElement("div");
    
    bin.className = "bin";
    bin.id        = `bin:${product.id}`;

    bin.setAttribute("ondrop",      "putBackItem(event)");
    bin.setAttribute("ondragover",  "dragoverHandler(event)");

    let head = newHead(product);
    bin.appendChild(head);

    return bin;
}

function stockBin(bin, product) {

    function newSaleItem(product, n) {
        let item = document.createElement("span");

        item.className   = "produce";
        item.id          = `${product.id}-${n}`;
        item.textContent = product.img;

        item.setAttribute("draggable",      "true");
        item.setAttribute("ondragstart",    "dragstartHandler(event)");
        item.setAttribute("data-price",     product.price);
        item.setAttribute("data-location",  "bin");

        console.log(`Creating produce item ${item.id}`);
        console.log(item);

        return item;
    }

    function getRandomStartAmount() {
        return Math.floor(Math.random() * 8 + 1);
    }

    let max = getRandomStartAmount();
    console.log(`Creating ${max} items`);

    for (let i = 0; i < max; ++i) {
        let newItem = newSaleItem(product, i + 1);
        bin.appendChild(newItem);
        console.log(`Added item to bin x ${i + 1}`);
    }

    return bin;
}

function stockShelf(inventory) {
   
    function newStockedBin(product) {
        let bin = newBin(product);
        return stockBin(bin, product);
    }

    let shelf = document.querySelector("div.shelf");

    inventory.forEach((product) => {
        let bin = newStockedBin(product);
        shelf.appendChild(bin);
        console.log(`Added bin ${bin.id} to shelf`);
    });
}

document.addEventListener("DOMContentLoaded", (event) => {
    stockShelf(inventory);
});

function dragstartHandler(ev) {
    // Add the target element's id to the data transfer object
    ev.dataTransfer.setData("text", ev.target.id);
    ev.dataTransfer.effectAllowed = "move";
}
function dragoverHandler(ev) {
    ev.preventDefault();
    ev.dataTransfer.dropEffect = "move";
}

function setTotalStatus(s) {
    let total = document.querySelector("p.total");
    total.dataset.status = s;
}
function markTotalValid() {
    console.log("Total is up to date");
    setTotalStatus("valid");
}
function markTotalInvalid() {
    console.log("Total needs updating");
    setTotalStatus("invalid");
}


function updateLocation(item, target) {
    item.dataset.location = target.className;
}
function isValidCartDrop(item, target) {
    return item.dataset.location !== target.className;
}

function dropInCart(ev) {
    ev.preventDefault();
    // Get the ID of the target and add the moved element to the target's DOM
    let data = ev.dataTransfer.getData("text");
    let item = document.getElementById(data);

    console.log("Try to put item in cart");
    console.log(item);

    if (isValidCartDrop(item, ev.target)) {
        updateLocation(item, ev.target);
        
        let cart = document.querySelector("div.cart");
        cart.appendChild(item);
        
        console.log(`Dragged and moved item to ${ev.target.className}`);
        markTotalInvalid();
    } else {
        console.log(`Item already in ${ev.target.className}`);
    }
}

function getBinIdFromItem(item) {
    return "bin:" + item.id.split("-")[0];
}

function putBackItem(ev) {
    ev.preventDefault();
    // Get the ID of the target and add the moved element to the target's DOM
    let data = ev.dataTransfer.getData("text");
    let item = document.getElementById(data);

    if (isValidCartDrop(item, ev.target)) {
        updateLocation(item, ev.target);

        let matchID = getBinIdFromItem(item);
        let bin = document.getElementById(matchID);
        bin.appendChild(item);
        console.log(`Dragged and moved item to ${ev.target.className}`);
    } else {
        console.log(`Item already in ${ev.target.className}`);
    }
}

function updatePrice(f) {
    let totalNum = document.getElementById("totalUSD");
    totalNum.textContent = formatPrice(f);
    markTotalValid();
}

function checkout() {
    console.log("Checking out: Calculating sum of prices");
    let produce = document.querySelectorAll("div.cart > span.produce");
    let prices = [];
    produce.forEach((item) => 
        prices.push(Number(item.dataset.price))
    );
    const sum = prices.reduce((a, b) => a + b, 0)
    updatePrice(sum);
}

function emptyCart() {
    console.log("Emptying cart");
    let cartItems = document.querySelectorAll("div.cart > span.produce");
    cartItems.forEach((item) => {
        let thisID = item.id;
        let matchID = getBinIdFromItem(item);
        console.log(`Adding item back to ${matchID}`);
        let bin = document.getElementById(matchID);
        bin.appendChild(item);
    });
    updatePrice(0);
}
