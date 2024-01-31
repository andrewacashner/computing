/* Encode and decode a message in a given encoding
 *
 * Reads single chars and matches them to given strings; matches can be single
 * or multi-char
 *
 * Andrew Cashner
 * 2023/03/09
 *
 * Encodings by Ben and Joy Cashner
 */

const alphanum = {
    type : "multi",
    dict : {
        " " : "-",
        a : "0",
        b : "1",
        c : "2",
        d : "3",
        e : "4",
        f : "5",
        g : "6",
        h : "7",
        i : "8",
        j : "9",
        k : "10",
        l : "11",
        m : "12",
        n : "13",
        o : "14",
        p : "15",
        q : "16",
        r : "17",
        s : "18",
        t : "19",
        u : "20",
        v : "21",
        w : "22",
        x : "23",
        y : "24",
        z : "25",
    }
}

const beetle = {
    type : "single",
    dict : {
        a : "u",
        b : "v",
        c : "w",
        d : "x",
        e : "y",
        f : "z",
        g : "a",
        h : "b",
        i : "c",
        j : "d",
        k : "e",
        l : "f",
        m : "g",
        n : "h",
        o : "i",
        p : "j",
        q : "k",
        r : "l",
        s : "m",
        t : "n",
        u : "o",
        v : "p",
        w : "q",
        x : "r",
        y : "s",
        z : "t",
    }
};


const forbidden = {
    type : "multi",
    dict : {
        " " : "-",
        a : "si",
        b : "wa",
        c : "la",
        d : "ni",
        e : "lu",
        f : "mi",
        g : "yi",
        h : "he",
        i : "in",
        j : "ge",
        k : "ki",
        l : "li",
        m : "ma",
        n : "nu",
        o : "ot",
        p : "pe",
        q : "un",
        r : "ri",
        s : "swa",
        t : "ti",
        u : "uv",
        v : "vi",
        w : "wu",
        x : "ya",
        y : "pu",
        z : "zi"
    }
}

const catalog = {
    "alphanum"  : alphanum,
    "beetle"    : beetle,
    "forbidden" : forbidden
}

function encode_one(encoding, item) {
    var match = item;
    if (encoding.dict.hasOwnProperty(item)) {
        match = encoding.dict[item];
    } 
    return match;
}

function separator(encoding, mode, stage) {
    const multi_delims = {
        encode : {
            read : "",
            write : " "
        },
        decode : {
            read : " ",
            write : ""
        }
    }

    var sep = "";
    if (encoding.type == "multi") {
        sep = multi_delims[mode][stage];
    }
    return sep;
}

function encode(encoding, message, mode = "encode") {
    var delim_read = separator(encoding, mode, "read");
    var delim_write = separator(encoding, mode, "write");
    var string_list = message.toLowerCase().split(delim_read);
    var match_list = string_list.map((item) => encode_one(encoding, item));
    var output = match_list.join(delim_write);
    console.log("Encoding result: " + output);
    return output;
}

function flip_dictionary(encoding) {
    var entries = Object.entries(encoding.dict).map(([key, value]) => [value, key]);
    var new_dict = Object.fromEntries(entries);
    var new_encoding = {
        type : encoding.type,
        dict : new_dict
    }
    return new_encoding; 
}

function decode(encoding, message) {
    return encode(flip_dictionary(encoding), message, "decode");
}

function process_form(fn, mode) {
    var inputText = document.getElementById("inputText").value;
    var encoding = document.querySelector('input[name="encoding"]:checked').value;
    console.log("Selected encoding " + encoding);

    var dictionary = catalog[encoding];
    var outputText = fn(dictionary, inputText, mode);
    document.getElementById("outputText").innerHTML = outputText;
    return true;
}

function encode_form() {
    process_form(encode);
}

function decode_form() {
    process_form(decode);
}

