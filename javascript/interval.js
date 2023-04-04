function item_value(el) {
    return el.value;
}

function show_selection(A, B) {
    const itemA = document.getElementById(A);
    const itemB = document.getElementById(B);
    itemB.innerHTML = itemA.value;
}

function setup_selection(select_id, show_fn) {
    const select = document.getElementById(select_id);
    select.addEventListener("click", show_fn);
};
    
function show_pitch() {
    show_selection("pitch-name-select", "input-pitch-name");
};
setup_selection("pitch-name-select", show_pitch);

function show_accidental() {
    show_selection("accidental-select", "input-pitch-accidental");
};
setup_selection("accidental-select", show_accidental);

function show_octave() {
    show_selection("addition-starting-pitch-octave", "input-pitch-octave");
};
setup_selection("addition-starting-pitch-octave", show_octave);

function show_operation() {
    show_selection("operation-select", "input-operation");
};
setup_selection("operation-select", show_operation);

function show_distance() {
    show_selection("quantity", "input-interval-distance");
};
setup_selection("quantity", show_distance);

function show_quality() {
    show_selection("quality-select", "input-interval-quality");
};
setup_selection("quality-select", show_quality);


function find_pitch() {
    const output_pitch_name = document.getElementById("output-pitch-name");
    output_pitch_name.innerHTML = document.getElementById("input-pitch-name").innerHTML;
    output_pitch_name.style.background = "red"; // TODO
}
const pitch_button = document.getElementById("find-pitch");
pitch_button.addEventListener("click", find_pitch);

/*
 * TODO
 * - Make a pitch class that can be passed as an argument or returned
 * - Make an interval class
 * - Make a function that takes a pitch class and an interval class and returns
 *   the result of pitch + interval
 * - Display results
 */
