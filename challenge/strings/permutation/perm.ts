function registerLetters(
    str: string, 
    inventory: object = {}, 
    increment: number = 1
): object {

    for (let c of str) {
        inventory[c] = c in inventory ? inventory[c] + increment : increment;
    }
    return inventory;
}

function checkLetters(str: string, inventory: object): object {
    return registerLetters(str, inventory, -1);
}

function compareLetterInventories(s1: string, s2: string): object {
    return checkLetters(s2, registerLetters(s1));
}


function isPermutation(baseStr: string, compareStr: string): boolean {
    let comparison = compareLetterInventories(baseStr, compareStr);
    return Object.values(comparison).every(c => c == 0);
}

function test(s1: string, s2: string): void {
    console.log(`${s1} vs ${s2}: ${isPermutation(s1, s2)}`);
}

let testValues = [
    ["abba", "baab"],
    ["dad", "add"],
    ["bacon", "eggs"],
    ["shut", "tush"]
];

testValues.forEach(([a, b]) => test(a, b));
