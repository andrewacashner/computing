/* Caesar Cipher */

function shift(c: string, shiftAmount: number): string {
    let alphabet: string;
    
    let numbers: string = "0123456789";

    if (numbers.includes(c)) {
        alphabet = numbers;
    } else {
        let alphabetLower: string = "abcdefghijklmnopqrstuvwxyz";
        let alphabetUpper: string = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        alphabet = c.toUpperCase() === c ? alphabetUpper : alphabetLower;
    }

    let shifted: string = c;    // No change if not in alphabet
    let index: number = alphabet.indexOf(c); 

    if (index >= 0) {
        index += shiftAmount;

        if (index < 0) {
            index += alphabet.length;
        } else if (index >= alphabet.length) {
            index -= alphabet.length;
        }

        shifted = alphabet[index];
        console.log(`${c} -> ${shifted}`);
    } 

    return shifted;
}

function caesarEncrypt(plaintext: string, shiftAmount: number): string {
    return plaintext.split("").map(c => shift(c, shiftAmount)).join("");
}

function caesarDecrypt(ciphertext: string, shiftAmount: number): string {
    return caesarEncrypt(ciphertext, -shiftAmount);
}

let g_key = 0;
let keyForm = document.getElementById("form:key");
let keyInput = document.getElementById("input:key") as HTMLInputElement;

keyForm.addEventListener("input", (event) => {
    g_key = parseInt(keyInput.value);
    console.log(`New key: ${g_key}`);
});

let encryptButton = document.getElementById("button:encrypt");
let decryptButton = document.getElementById("button:decrypt");

let message = document.getElementById("input:message") as HTMLInputElement;
let outputArea = document.getElementById("outputArea");

function crypt(
    name: string, 
    fn: (msg: string, key: number) => string,
    msg: string,
    key: number
): void {
    console.log(name);
    let output = fn(msg, key);
    console.log(output);
    outputArea.textContent = output;
}

let inputForm = document.getElementById("sec:caesar:input");
inputForm.addEventListener("submit", (event) => event.preventDefault());

encryptButton.addEventListener("click", () => crypt("encrypt", caesarEncrypt, 
                                                    message.value, g_key));

decryptButton.addEventListener("click", () => crypt("decrypt", caesarDecrypt, 
                                                    message.value, g_key));

let copyButton = document.getElementById("button:copyBack");
copyButton.addEventListener("click", () => {
    console.log("Copy output to input");
    message.value = outputArea.textContent;
});

