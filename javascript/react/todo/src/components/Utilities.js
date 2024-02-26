export default class Utilities {
  static partition(array, test) {
    let results = { true: [], false: [] };
    array.forEach(i => results[test(i)].push(i));
    return [results.true, results.false];
  }
}


