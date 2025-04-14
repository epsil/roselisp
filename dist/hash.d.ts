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
/**
 * Whether something is a hash map.
 *
 * Similar to [`hash?` in Racket][rkt:hashp].
 *
 * [rkt:hashp]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash~3f%29%29
 */
declare function hashp_(v: any): any;
declare namespace hashp_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Make a hash map from a list of `(key . value)` pairs.
 *
 * Similar to [`make-hash` in Racket][rkt:make-hash].
 *
 * [rkt:make-hash]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._make-hash%29%29
 */
declare function makeHash_(assocs?: any): any;
declare namespace makeHash_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | never[])[])[])[])[];
}
/**
 * Set `key` to `v` in the hash map `ht`.
 *
 * Similar to [`hash-set!` in Racket][rkt:hash-set-x].
 *
 * [rkt:hash-set-x]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-set%21%29%29
 */
declare function hashSetX_(ht: any, key: any, v: any): any;
declare namespace hashSetX_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Set `key` to `v` in the hash map `ht`,
 * returning a new hash map.
 *
 * Similar to [`hash-set` in Racket][rkt:hash-set].
 *
 * [rkt:hash-set]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-set%29%29
 */
declare function hashSet_(ht: any, key: any, v: any): any;
declare namespace hashSet_ {
    var lispSource: (symbol | (symbol | symbol[] | (symbol | symbol[])[][])[])[];
}
/**
 * Get the value of `key` in the hash map `ht`.
 * `failure-result`, if specified, is returned
 * if there is no value.
 *
 * Similar to [`hash-ref` in Racket][rkt:hash-ref].
 *
 * [rkt:hash-ref]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-ref%29%29
 */
declare function hashRef_(ht: any, key: any, failureResult?: any): any;
declare namespace hashRef_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
/**
 * Whether a hash map has a value for a given key.
 *
 * Similar to [`hash-has-key?` in Racket][rkt:hash-has-key-p].
 *
 * [rkt-hash-has-key-p]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28lib._racket%2Fprivate%2Fmore-scheme..rkt%29._hash-has-key~3f%29%29
 */
declare function hashHasKeyP_(ht: any, key: any): any;
declare namespace hashHasKeyP_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Remove the value for a given key in a hash map
 *
 * Similar to [`hash-remove!` in Racket][rkt:hash-remove-x].
 *
 * [rkt:hash-remove-x]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-remove%21%29%29
 */
declare function hashRemove_(ht: any, key: any): any;
declare namespace hashRemove_ {
    var lispSource: (symbol | (symbol | symbol[] | (symbol | symbol[])[][])[])[];
}
/**
 * Remove the value for a given key in a hash map,
 * returning a new hash map.
 *
 * Similar to [`hash-remove` in Racket][rkt:hash-remove].
 *
 * [rkt:hash-remove]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-remove%29%29
 */
declare function hashRemoveX_(ht: any, key: any): any;
declare namespace hashRemoveX_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return the number of keys in a hash table.
 */
declare function hashSize_(ht: any): any;
declare namespace hashSize_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Clone a hash map.
 *
 * Similar to [`hash-copy` in Racket][rkt:hash-copy].
 *
 * [rkt:hash-copy]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-copy%29%29
 */
declare function hashCopy_(ht: any): any;
declare namespace hashCopy_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Delete all entries in a hash map,
 * returning a new hash map.
 *
 * Similar to [`hash-clear` in Racket][rkt:hash-clear].
 *
 * [rkt:hash-clear]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-clear%29%29
 */
declare function hashClear_(ht: any): any;
declare namespace hashClear_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Delete all entries in a hash map.
 *
 * Similar to [`hash-clear!` in Racket][rkt:hash-clear-x].
 *
 * [rkt:hash-clear-x]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28quote._~23~25kernel%29._hash-clear%21%29%29
 */
declare function hashClearX_(ht: any): any;
declare namespace hashClearX_ {
    var lispSource: (symbol | symbol[])[];
}
/**
 * Return a list of all the keys in a hash map.
 *
 * Similar to [`hash-keys` in Racket][rkt:hash-keys].
 *
 * [rkt:hash-keys]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._hash-keys%29%29
 */
declare function hashKeys_(ht: any): any;
declare namespace hashKeys_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[][])[])[];
}
/**
 * Return a list of all the values in a hash map.
 *
 * Similar to [`hash-values` in Racket][rkt:hash-values].
 *
 * [rkt:hash-values]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._hash-keys%29%29
 */
declare function hashValues_(ht: any): any;
declare namespace hashValues_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[][])[])[];
}
/**
 * Convert a hash map to a list of `(key value)` tuples.
 */
declare function hashEntries_(ht: any): any;
declare namespace hashEntries_ {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[][])[])[];
}
/**
 * Convert a hash map to a list of `(key . value)` pairs.
 *
 * Similar to [`hash->list` in Racket][rkt:hash-to-list].
 *
 * [rkt:hash-to-list]: https://docs.racket-lang.org/reference/hashtables.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._hash-~3elist%29%29
 */
declare function hashToList_(ht: any): any;
declare namespace hashToList_ {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[];
}
export { hashToList_, hashClearX_, hashClear_, hashCopy_, hashEntries_, hashHasKeyP_, hashKeys_, hashRef_, hashRemoveX_, hashRemove_, hashSetX_, hashSet_, hashSize_, hashValues_, hashp_, makeHash_ };
