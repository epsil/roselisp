/**
 * # Memoization
 *
 * Memoization implementation.
 *
 * ## Description
 *
 * Utilities for [memoizing][w:Memoization] functions.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * [w:Memoization]: https://en.wikipedia.org/wiki/Memoization
 */
/**
 * End-of-sequence marker. Special value used as a key
 * in the cache map.
 *
 * It is implemented as a unique [`Symbol`][js:Symbol] to prevent
 * collision with cached values. Avoid using this value in code;
 * it is exported for testing purposes only.
 *
 * [js:Symbol]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Symbol
 */
declare const eof: any;
/**
 * Make a [memoized][w:Memoization] version of the function `f`.
 * Functions of any arity may be memoized.
 *
 * Returns a function wrapper around `f` that caches the values
 * produced by `f`. The memoization cache is exposed as the
 * `cache` property on the memoized function.
 *
 * [w:Memoization]: https://en.wikipedia.org/wiki/Memoization
 */
declare function memoize(f: any, cache?: any): any;
declare namespace memoize {
    var lispSource: (symbol | (symbol | (symbol | symbol[])[])[])[];
}
/**
 * Make a [memoized][w:Memoization] version of the function `f`.
 *
 * Returns a tuple `(memoized-f cache)`, where `memoized-f` is
 * the memoized function and `cache` is the memoization cache.
 * The memoization cache is also exposed as the `cache` property
 * on the memoized function.
 *
 * It is possible to specify a custom memoization cache with the
 * optional `cache` argument. This value must be an instance of
 * [`Map`][js:Map], or an object that implements the same interface as
 * `Map` (i.e., `.has()`, `.get()` and `.set()`). This is useful if
 * one wants value equality to be based on something else than the
 * [SameValueZero][js:SameValueZero] algorithm, which is what `Map`
 * uses.
 *
 * [w:Memoization]: https://en.wikipedia.org/wiki/Memoization
 * [js:Map]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Map
 * [js:SameValueZero]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Equality_comparisons_and_sameness#same-value-zero_equality
 */
declare function memoize2(f: any, cache?: any): any;
declare namespace memoize2 {
    var lispSource: (symbol | (symbol | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[];
}
/**
 * Return a [memoized][w:Memoization] version of the function `f`,
 * where the memoization cache is passed as the last argument.
 *
 * [w:Memoization]: https://en.wikipedia.org/wiki/Memoization
 */
declare function memoizeWithArg(f: any, arity?: any): any;
declare namespace memoizeWithArg {
    var lispSource: (symbol | (symbol | (symbol | (symbol | symbol[])[] | ((symbol | (number | symbol)[])[] | (symbol | (symbol | (symbol | symbol[])[])[])[])[])[])[])[];
}
export { eof, memoize, memoize2, memoizeWithArg };
