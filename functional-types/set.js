
/*

Emulate a Set with only functions of the type
  item => boolean
returning whether the item is in the set or not

*/

const empty_set = item => false;

const include = (set, included_item) => item => item === included_item || set(item);
const exclude = (set, excluded_item) => item => item !== excluded_item && set(item);

// --

let primes = empty_set;
primes = include(primes, 2);
primes = include(primes, 3);
primes = include(primes, 5);
primes = include(primes, 7);
primes = include(primes, 11);
primes = include(primes, 13);

primes = include(primes, 4);
// wait, that's not right!
// remove it!
primes = exclude(primes, 4);

const assert = bool => { if (!bool) throw Error(); };

for (let i = 0; i <= 13; i++) {
  const is_prime = [2, 3, 5, 7, 11, 13].includes(i);
  const in_set = primes(i);
  assert(is_prime === in_set);
}
