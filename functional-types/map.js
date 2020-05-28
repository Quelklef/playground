
/*

Emulate a Map with only functions of the type
  key => value
returning the value for the given key

*/

const empty_map = key => undefined;

const set = (map, set_key, value) => get_key => get_key === set_key ? value : map(get_key);
const unset = (map, unset_key) => set(map, unset_key, undefined);

// --

let colors = empty_map;
colors = set(colors, "Javascript", "yellow");
colors = set(colors, "Typescript", "blue");
colors = set(colors, "Ruby"      , "red");
colors = set(colors, "Python"    , "green");
colors = set(colors, "C"         , "???");

const assert = bool => { if (!bool) throw Error(); };

const pairs = {
  "Javascript": "yellow",
  "Typescript": "blue",
  "Ruby"      : "red",
  "Python"    : "green",
  "C"         : "???",
};
for (const [key, expected] of Object.entries(pairs)) {
  const actual = colors(key);
  assert(actual === expected);
}
