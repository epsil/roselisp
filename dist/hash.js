"use strict";
// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Hash maps
 *
 * Functions for working with hash maps.
 *
 * ## Description
 *
 * An implementation of hash maps, expressed in terms of JavaScript's
 * [`Map`][js:Map].
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
 */
Object.defineProperty(exports, "__esModule", { value: true });
exports.makeHash_ = exports.hashp_ = exports.hashValues_ = exports.hashSize_ = exports.hashSet_ = exports.hashSetX_ = exports.hashRemove_ = exports.hashRemoveX_ = exports.hashRef_ = exports.hashKeys_ = exports.hashHasKeyP_ = exports.hashEntries_ = exports.hashCopy_ = exports.hashClear_ = exports.hashClearX_ = exports.hashToList_ = void 0;
const [flatten, cons] = (() => {
    function flatten_(lst) {
        return lst.reduce(function (acc, x) {
            if (Array.isArray(x)) {
                return [...acc, ...flatten_(x)];
            }
            else if (x === Symbol.for('.')) {
                return acc;
            }
            else {
                acc.push(x);
                return acc;
            }
        }, []);
    }
    function cons_(x, y) {
        if (Array.isArray(y)) {
            return [x, ...y];
        }
        else {
            return [x, Symbol.for('.'), y];
        }
    }
    return [flatten_, cons_];
})();
/**
 * Whether something is a hash map.
 *
 * Similar to [`hash?` in Racket][rkt:hashp].
 *
 * [rkt:hashp]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash~3f%29%29
 */
function hashp_(v) {
    return v instanceof Map;
}
exports.hashp_ = hashp_;
hashp_.lispSource = [Symbol.for('define'), [Symbol.for('hash?_'), Symbol.for('v')], [Symbol.for('is-a?'), Symbol.for('v'), Symbol.for('Map')]];
/**
 * Make a hash map from a list of `(key . value)` pairs.
 *
 * Similar to [`make-hash` in Racket][rkt:make-hash].
 *
 * [rkt:make-hash]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._make-hash%29%29
 */
function makeHash_(assocs = []) {
    return new Map(assocs.map(function (x) {
        return flatten(x);
    }));
}
exports.makeHash_ = makeHash_;
makeHash_.lispSource = [Symbol.for('define'), [Symbol.for('make-hash_'), [Symbol.for('assocs'), [Symbol.for('quote'), []]]], [Symbol.for('new'), Symbol.for('Map'), [Symbol.for('map'), Symbol.for('flatten'), Symbol.for('assocs')]]];
/**
 * Set `key` to `v` in the hash map `ht`.
 *
 * Similar to [`hash-set!` in Racket][rkt:hash-set-x].
 *
 * [rkt:hash-set-x]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-set%21%29%29
 */
function hashSetX_(ht, key, v) {
    return ht.set(key, v);
}
exports.hashSetX_ = hashSetX_;
hashSetX_.lispSource = [Symbol.for('define'), [Symbol.for('hash-set!_'), Symbol.for('ht'), Symbol.for('key'), Symbol.for('v')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('set'), Symbol.for('key'), Symbol.for('v')]];
/**
 * Set `key` to `v` in the hash map `ht`,
 * returning a new hash map.
 *
 * Similar to [`hash-set` in Racket][rkt:hash-set].
 *
 * [rkt:hash-set]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-set%29%29
 */
function hashSet_(ht, key, v) {
    const result = new Map(ht);
    result.set(key, v);
    return result;
}
exports.hashSet_ = hashSet_;
hashSet_.lispSource = [Symbol.for('define'), [Symbol.for('hash-set_'), Symbol.for('ht'), Symbol.for('key'), Symbol.for('v')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('hash-copy'), Symbol.for('ht')]]], [Symbol.for('hash-set!'), Symbol.for('result'), Symbol.for('key'), Symbol.for('v')], Symbol.for('result')]];
/**
 * Get the value of `key` in the hash map `ht`.
 * `failure-result`, if specified, is returned
 * if there is no value.
 *
 * Similar to [`hash-ref` in Racket][rkt:hash-ref].
 *
 * [rkt:hash-ref]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-ref%29%29
 */
function hashRef_(ht, key, failureResult = undefined) {
    if ((failureResult !== undefined) && !ht.has(key)) {
        return failureResult;
    }
    else {
        return ht.get(key);
    }
}
exports.hashRef_ = hashRef_;
hashRef_.lispSource = [Symbol.for('define'), [Symbol.for('hash-ref_'), Symbol.for('ht'), Symbol.for('key'), [Symbol.for('failure-result'), Symbol.for('undefined')]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('not'), [Symbol.for('eq?'), Symbol.for('failure-result'), Symbol.for('undefined')]], [Symbol.for('not'), [Symbol.for('hash-has-key?'), Symbol.for('ht'), Symbol.for('key')]]], Symbol.for('failure-result')], [Symbol.for('else'), [Symbol.for('send'), Symbol.for('ht'), Symbol.for('get'), Symbol.for('key')]]]];
/**
 * Whether a hash map has a value for a given key.
 *
 * Similar to [`hash-has-key?` in Racket][rkt:hash-has-key-p].
 *
 * [rkt-hash-has-key-p]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._hash-has-key~3f%29%29
 */
function hashHasKeyP_(ht, key) {
    return ht.has(key);
}
exports.hashHasKeyP_ = hashHasKeyP_;
hashHasKeyP_.lispSource = [Symbol.for('define'), [Symbol.for('hash-has-key?_'), Symbol.for('ht'), Symbol.for('key')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('has'), Symbol.for('key')]];
/**
 * Remove the value for a given key in a hash map
 *
 * Similar to [`hash-remove!` in Racket][rkt:hash-remove-x].
 *
 * [rkt:hash-remove-x]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-remove%21%29%29
 */
function hashRemove_(ht, key) {
    const result = new Map(ht);
    result.delete(key);
    return result;
}
exports.hashRemove_ = hashRemove_;
hashRemove_.lispSource = [Symbol.for('define'), [Symbol.for('hash-remove_'), Symbol.for('ht'), Symbol.for('key')], [Symbol.for('let'), [[Symbol.for('result'), [Symbol.for('hash-copy'), Symbol.for('ht')]]], [Symbol.for('hash-remove!'), Symbol.for('result'), Symbol.for('key')], Symbol.for('result')]];
/**
 * Remove the value for a given key in a hash map,
 * returning a new hash map.
 *
 * Similar to [`hash-remove` in Racket][rkt:hash-remove].
 *
 * [rkt:hash-remove]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-remove%29%29
 */
function hashRemoveX_(ht, key) {
    return ht.delete(key);
}
exports.hashRemoveX_ = hashRemoveX_;
hashRemoveX_.lispSource = [Symbol.for('define'), [Symbol.for('hash-remove!_'), Symbol.for('ht'), Symbol.for('key')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('delete'), Symbol.for('key')]];
/**
 * Return the number of keys in a hash table.
 */
function hashSize_(ht) {
    return ht.size;
}
exports.hashSize_ = hashSize_;
hashSize_.lispSource = [Symbol.for('define'), [Symbol.for('hash-size_'), Symbol.for('ht')], [Symbol.for('get-field'), Symbol.for('size'), Symbol.for('ht')]];
/**
 * Clone a hash map.
 *
 * Similar to [`hash-copy` in Racket][rkt:hash-copy].
 *
 * [rkt:hash-copy]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-copy%29%29
 */
function hashCopy_(ht) {
    return new Map(ht);
}
exports.hashCopy_ = hashCopy_;
hashCopy_.lispSource = [Symbol.for('define'), [Symbol.for('hash-copy_'), Symbol.for('ht')], [Symbol.for('new'), Symbol.for('Map'), Symbol.for('ht')]];
/**
 * Delete all entries in a hash map,
 * returning a new hash map.
 *
 * Similar to [`hash-clear` in Racket][rkt:hash-clear].
 *
 * [rkt:hash-clear]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-clear%29%29
 */
function hashClear_(ht) {
    return new Map();
}
exports.hashClear_ = hashClear_;
hashClear_.lispSource = [Symbol.for('define'), [Symbol.for('hash-clear_'), Symbol.for('ht')], [Symbol.for('new'), Symbol.for('Map')]];
/**
 * Delete all entries in a hash map.
 *
 * Similar to [`hash-clear!` in Racket][rkt:hash-clear-x].
 *
 * [rkt:hash-clear-x]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-clear%21%29%29
 */
function hashClearX_(ht) {
    return ht.clear();
}
exports.hashClearX_ = hashClearX_;
hashClearX_.lispSource = [Symbol.for('define'), [Symbol.for('hash-clear!_'), Symbol.for('ht')], [Symbol.for('send'), Symbol.for('ht'), Symbol.for('clear')]];
/**
 * Return a list of all the keys in a hash map.
 *
 * Similar to [`hash-keys` in Racket][rkt:hash-keys].
 *
 * [rkt:hash-keys]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._hash-keys%29%29
 */
function hashKeys_(ht) {
    return [...ht.keys()];
}
exports.hashKeys_ = hashKeys_;
hashKeys_.lispSource = [Symbol.for('define'), [Symbol.for('hash-keys_'), Symbol.for('ht')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('ht'), Symbol.for('keys')]]]]];
/**
 * Return a list of all the values in a hash map.
 *
 * Similar to [`hash-values` in Racket][rkt:hash-values].
 *
 * [rkt:hash-values]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._hash-keys%29%29
 */
function hashValues_(ht) {
    return [...ht.values()];
}
exports.hashValues_ = hashValues_;
hashValues_.lispSource = [Symbol.for('define'), [Symbol.for('hash-values_'), Symbol.for('ht')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('ht'), Symbol.for('values')]]]]];
/**
 * Convert a hash map to a list of `(key value)` tuples.
 */
function hashEntries_(ht) {
    return [...ht.entries()];
}
exports.hashEntries_ = hashEntries_;
hashEntries_.lispSource = [Symbol.for('define'), [Symbol.for('hash-entries_'), Symbol.for('ht')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), [Symbol.for('send'), Symbol.for('ht'), Symbol.for('entries')]]]]];
/**
 * Convert a hash map to a list of `(key . value)` pairs.
 *
 * Similar to [`hash->list` in Racket][rkt:hash-to-list].
 *
 * [rkt:hash-to-list]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._hash-~3elist%29%29
 */
function hashToList_(ht) {
    return [...ht.entries()].map(function (x) {
        return cons(x[0], x[1]);
    });
}
exports.hashToList_ = hashToList_;
hashToList_.lispSource = [Symbol.for('define'), [Symbol.for('hash->list_'), Symbol.for('ht')], [Symbol.for('map'), [Symbol.for('lambda'), [Symbol.for('x')], [Symbol.for('cons'), [Symbol.for('array-first'), Symbol.for('x')], [Symbol.for('array-second'), Symbol.for('x')]]], [Symbol.for('hash-entries'), Symbol.for('ht')]]];
