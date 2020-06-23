
// Represent a JS object as a chain of object diffs

// Empty object
const empty = undefined;

// Naive implementation

// Invariants:
// - diff.parent : diff | undefind
// - diff.with : object | undefind
// - diff.without: array | undefind
// - Object.keys(diff.with) and diff.without are disjoint

function set(obj, prop, val) {
  return {
      parent: obj,
      with: { [prop]: val },
  };
}

function del(obj, prop) {
  return {
    parent: obj,
    without: [prop],
  }
}

function keys(obj) {
  if (obj === undefined) return new Set();
  const keyz = keys(obj.parent);
  if (obj.with)
    Object.keys(obj.with).forEach(key => keyz.add(key));
  if (obj.without)
    obj.without.forEach(key => keyz.delete(key));
  return keyz;
}

function has(obj, prop) {
  if (obj === undefined) return false;
  return (obj.with && obj.with.includes(prop)) || has(obj.parent, prop);
}

function get(obj, prop) {
  if (obj === undefined) return undefined;
  if (obj.without && obj.without.includes(prop)) return undefined;
  if (prop in obj.with) return obj.with[prop];
  return get(obj.parent, prop);
}


{
  
  const assert = bool => { if (!bool) throw Error(); };

  let o = empty;

  assert(get(o, 'x') === undefined);
  assert(!has(o, 'x'));
  assert(keys(o).size === 0);

  o = set(o, 'x', 10);

  assert(get(o, 'x') === 10);
  assert(keys(o).size === 1);
  assert(keys(o).has('x'));

  o = set(o, 'y', 20);
  o = set(o, 'z', 30);
  
  assert(get(o, 'y') === 20);
  assert(get(o, 'z') === 30);
  assert(keys(o).size === 3);
  assert(keys(o).has('y'));
  assert(keys(o).has('z'));

  o = del(o, 'y');

  assert(keys(o).size === 2);
  assert(!keys(o).has('y'));

  console.log('ok');

}
