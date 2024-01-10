function formatPrice(n) {
    return "$" + parseFloat(n).toFixed(2);
}

function getRandomStartAmount() {
    return Math.floor(Math.random() * 8 + 1);
}

document.addEventListener("DOMContentLoaded", (event) => {
    var bins = document.querySelectorAll("div.bin");
    bins.forEach((bin) => {
        console.log("Setting up bin " + bin.id);
        bin.setAttribute("ondrop", "putBackItem(event)");
        bin.setAttribute("ondragover", "dragoverHandler(event)");

        var head = document.createElement("h1");
        head.textContent = bin.dataset.label;
        var priceLabel = document.createElement("span");
        priceLabel.textContent = formatPrice(bin.dataset.price);
        head.appendChild(priceLabel);
        bin.appendChild(head);
        console.log("Created bin label");
        console.log(head);

        var item = document.createElement("span");
        item.className = "produce";
        item.id = bin.id.replace("bin:", "");
        item.setAttribute("data-price", bin.dataset.price);
        item.setAttribute("draggable", "true");
        item.setAttribute("ondragstart", "dragstartHandler(event)");
        item.setAttribute("data-incart", "false");
        item.textContent = bin.dataset.img;
        console.log("Creating produce item " + bin.id.replace("bin:", ""));
        console.log(item);

        let max = getRandomStartAmount();
        console.log("Creating " + max + " items");
        for (let i = 0; i < max; ++i) {
            let newItem = item.cloneNode(true);
            newItem.id = newItem.id + "-" +  (i + 1);
            bin.appendChild(newItem);
            console.log("Added item to bin x" + (i + 1));
        }
    });
});

//        <h1>Potato</h1>
//        <span class="produce" id="potato" 
//              ondragstart="dragstartHandler(event)">🥔</span>
//        <span class="produce" id="potato" ...</span>
//        ...

function dragstartHandler(ev) {
    // Add the target element's id to the data transfer object
    ev.dataTransfer.setData("application/my-app", ev.target.id);
    ev.dataTransfer.effectAllowed = "move";
}
function dragoverHandler(ev) {
    ev.preventDefault();
    ev.dataTransfer.dropEffect = "move";
}
function dropHandler(ev) {
    ev.preventDefault();
    // Get the ID of the target and add the moved element to the target's DOM
    const data = ev.dataTransfer.getData("application/my-app");
    let item = document.getElementById(data);
    console.log("Try to put item in cart");
    console.log(item);
    if (item.dataset.incart == "false") {
        item.dataset.incart = "true";
        ev.target.appendChild(item);
        console.log("Dragged and moved item");
    } else {
        console.log("Item already in cart");
    }
}
function getBinIdFromItem(item) {
    return "bin:" + item.id.split("-")[0];
}
function putBackItem(ev) {
    ev.preventDefault();
    // Get the ID of the target and add the moved element to the target's DOM
    const data = ev.dataTransfer.getData("application/my-app");
    let item = document.getElementById(data);
    if (item.dataset.incart == "true") {
        console.log("Try to put item back");
        console.log(item);
        item.dataset.incart = "false";
        let matchID = getBinIdFromItem(item);
        let bin = document.getElementById(matchID);
        bin.appendChild(item);
        console.log("Dragged and moved item");
    } else {
        console.log("Can't move item within bin");
    }
}


function updatePrice(f) {
    var total = document.getElementById("total");
    total.textContent = formatPrice(f);
}
function checkout() {
    console.log("Checking out: Calculating sum of prices");
    var produce = document.querySelectorAll("div.cart > span.produce");
    var prices = [];
    produce.forEach((item) => 
        prices.push(Number(item.dataset.price))
    );
    const sum = prices.reduce((a, b) => a + b, 0)
    updatePrice(sum);
}

function emptyCart() {
    console.log("Emptying cart");
    var cartItems = document.querySelectorAll("div.cart > span.produce");
    cartItems.forEach((item) => {
        let thisID = item.id;
        let matchID = getBinIdFromItem(item);
        console.log("Adding item back to " + matchID);
        var bin = document.getElementById(matchID);
        bin.appendChild(item);
    });
    updatePrice(0);
}
