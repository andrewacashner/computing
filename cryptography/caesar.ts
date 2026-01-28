/* Caesar Cipher */
"use strict";

class CaesarCipher {
    #key = 0;

    constructor(key : number = 0) {
        this.#key = key;
        console.log(`Created new cipher with key ${this.#key}`)
    }

    setKey = (key: number): void => {
        this.#key = key;
        console.log(`Set new key to ${this.#key}`);
    }

    static #alphabetLower = "abcdefghijklmnopqrstuvwxyz";
    static #alphabetUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    static #numbers       = "0123456789";

    #selectAlphabet = (c: string): string => {
        let alphabet: string;

        if (CaesarCipher.#numbers.includes(c)) {
            alphabet = CaesarCipher.#numbers;
        } else {
            alphabet = (c.toUpperCase() === c)
                        ? CaesarCipher.#alphabetUpper 
                        : CaesarCipher.#alphabetLower;
        }

        return alphabet;
    }

    #shift = (c: string, shiftAmount: number): string => {
        let alphabet: string = this.#selectAlphabet(c);

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
            // console.log(`${c} -> ${shifted}`);
        } 

        return shifted;
    }

    #caesarEncrypt = (shiftAmount: number, msg: string): string => 
        msg.split("").map(c => this.#shift(c, shiftAmount)).join("");

    encrypt = (msg: string) => this.#caesarEncrypt(this.#key, msg);
    decrypt = (msg: string) => this.#caesarEncrypt(-this.#key, msg);

    
}

function requestCrypto(action: string, 
                       msg: string, 
                       element: HTMLElement): void {
    let cryptEvent = new CustomEvent(action, { detail: { "msg": msg }});
    console.log(`Sending message '${msg}' for action '${action}'`);
    element.dispatchEvent(cryptEvent);
}

window.addEventListener("load", () => {
        globalThis.cipher = new CaesarCipher();
        setupKeyForm();
        setupTextForm();
});

function setupKeyForm(): void {
    let keyForm = document.getElementById("form:key");
    let keyInput = document.getElementById("input:key") as HTMLInputElement;

    keyForm.addEventListener("input", () => 
                             globalThis.cipher.setKey(parseInt(keyInput.value)));
}

function setupTextForm(): void {
    let encryptButton = document.getElementById("button:encrypt");
    let decryptButton = document.getElementById("button:decrypt");

    let message = document.getElementById("input:message") as HTMLInputElement;
    let outputArea = document.getElementById("outputArea");

    let inputForm = document.getElementById("sec:caesar:input");
    inputForm.addEventListener("submit", (event) => event.preventDefault());

    encryptButton.addEventListener("click", () => 
                                   requestCrypto("encrypt", message.value, outputArea));

    decryptButton.addEventListener("click", () => 
                                   requestCrypto("decrypt", message.value, outputArea));

    outputArea.addEventListener("encrypt", (event: CustomEventInit<string>) => {
        console.log("Encrypt message received");
        let plaintext = event.detail["msg"];
        let ciphertext = globalThis.cipher.encrypt(plaintext);
        outputArea.textContent = ciphertext;
    });
    
    outputArea.addEventListener("decrypt", (event: CustomEventInit<string>) => {
        console.log("Decrypt message received");
        let ciphertext = event.detail["msg"];
        let plaintext = globalThis.cipher.decrypt(ciphertext);
        outputArea.textContent = plaintext;
    });

    let copyButton = document.getElementById("button:copyBack");
    copyButton.addEventListener("click", () => {
        console.log("Copy output to input");
        message.value = outputArea.textContent;
    });
}

