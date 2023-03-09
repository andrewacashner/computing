// TODO deal with one vs multi char encodings

const beetle = {
    "a" : "u",
    "b" : "v",
    "c" : "w",
    "d" : "x",
    "e" : "y",
    "f" : "z",
    "g" : "a",
    "h" : "b",
    "i" : "c",
    "j" : "d",
    "k" : "e",
    "l" : "f",
    "m" : "g",
    "n" : "h",
    "o" : "i",
    "p" : "j",
    "q" : "k",
    "r" : "l",
    "s" : "m",
    "t" : "n",
    "u" : "o",
    "v" : "p",
    "w" : "q",
    "x" : "r",
    "y" : "s",
    "z" : "t",
};

const alphanum = {
    "a" : "0",
    "b" : "1",
    "c" : "2",
    "d" : "3",
    "e" : "4",
    "f" : "5",
    "g" : "6",
    "h" : "7",
    "i" : "8",
    "j" : "9",
    "k" : "10",
    "l" : "11",
    "m" : "12",
    "n" : "13",
    "o" : "14",
    "p" : "15",
    "q" : "16",
    "r" : "17",
    "s" : "18",
    "t" : "19",
    "u" : "20",
    "v" : "21",
    "w" : "22",
    "x" : "23",
    "y" : "24",
    "z" : "25",
}

const encoding_catalog = {
    "beetle"   : beetle,
    "alphanum" : alphanum
};


function encode_one(encoding, item) {
    var match = item;
    if (encoding.hasOwnProperty(item)) {
        match = encoding[item];
    } 
    return match;
}

function max_length(array) {
    var lengths = array.map((item) => item.length);
    return Math.max(...lengths);
}

function formatted_message(string_list) {
    var join_char = '';
    if (max_length(string_list) > 1) {
        var join_char = ' ';
    } 
    return string_list.join(join_char);
}

function encode(encoding, message) {
    var string_list = message.toLowerCase().split('');
    var match_list = string_list.map((item) => encode_one(encoding, item));
    console.log('Encoding result: ' + formatted_message(match_list));
    return formatted_message(match_list);
}

function flip_dictionary(obj) {
    var entries = Object.entries(obj).map(([key, value]) => [value, key]);
    return Object.fromEntries(entries);
}

function decode(encoding, message) {
    var reverse_encoding = flip_dictionary(encoding);
    return encode(reverse_encoding, message);
}

function process_form(fn) {
    var inputText = document.getElementById("inputText").value;
    var encoding = document.querySelector('input[name="encoding"]:checked').value;
    console.log("Selected encoding " + encoding);

    var outputText = fn(encoding_catalog[encoding], inputText);
    document.getElementById("outputText").innerHTML = outputText;
    return true;
}

function encode_form() {
    process_form(encode);
}

function decode_form() {
    process_form(decode);
}

