// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
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

import {
  mapGetTuple,
  mapSetX
} from './util';

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
const eof: any = Symbol('eof');

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
function memoize(f: any, cache: any = new Map()): any {
  const [memoizedF]: any[] = memoize2(f, cache);
  return memoizedF;
}

memoize.lispSource = [Symbol.for('define'), [Symbol.for('memoize'), Symbol.for('f'), [Symbol.for('cache'), [Symbol.for('make-hash')]]], [Symbol.for('define-values'), [Symbol.for('memoized-f')], [Symbol.for('memoize2'), Symbol.for('f'), Symbol.for('cache')]], Symbol.for('memoized-f')];

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
function memoize2(f: any, cache: any = new Map()): any {
  function memoizedF(...args: any[]): any {
    const map: any = getCache(memoizedF);
    const path: any = makePath(args);
    let [value, hasValue]: any[] = mapGetTuple(map, path);
    if (hasValue) {
      return value;
    } else {
      value = f(...args);
      mapSetX(map, path, value);
      return value;
    }
  }
  memoizedF.lispSource = [Symbol.for('define'), [Symbol.for('memoized-f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('map'), [Symbol.for('get-cache'), Symbol.for('memoized-f')]], [Symbol.for('define'), Symbol.for('path'), [Symbol.for('make-path'), Symbol.for('args')]], [Symbol.for('define-values'), [Symbol.for('value'), Symbol.for('has-value')], [Symbol.for('map-get-tuple'), Symbol.for('map'), Symbol.for('path')]], [Symbol.for('cond'), [Symbol.for('has-value'), Symbol.for('value')], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('value'), [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]], [Symbol.for('map-set!'), Symbol.for('map'), Symbol.for('path'), Symbol.for('value')], Symbol.for('value')]]];
  setCacheX(memoizedF, cache);
  return [memoizedF, cache];
}

memoize2.lispSource = [Symbol.for('define'), [Symbol.for('memoize2'), Symbol.for('f'), [Symbol.for('cache'), [Symbol.for('make-hash')]]], [Symbol.for('define'), [Symbol.for('memoized-f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('map'), [Symbol.for('get-cache'), Symbol.for('memoized-f')]], [Symbol.for('define'), Symbol.for('path'), [Symbol.for('make-path'), Symbol.for('args')]], [Symbol.for('define-values'), [Symbol.for('value'), Symbol.for('has-value')], [Symbol.for('map-get-tuple'), Symbol.for('map'), Symbol.for('path')]], [Symbol.for('cond'), [Symbol.for('has-value'), Symbol.for('value')], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('value'), [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]], [Symbol.for('map-set!'), Symbol.for('map'), Symbol.for('path'), Symbol.for('value')], Symbol.for('value')]]], [Symbol.for('set-cache!'), Symbol.for('memoized-f'), Symbol.for('cache')], [Symbol.for('values'), Symbol.for('memoized-f'), Symbol.for('cache')]];

/**
 * Get the memoization cache of `f`.
 */
function getCache(f: any): any {
  return f.cache;
}

getCache.lispSource = [Symbol.for('define'), [Symbol.for('get-cache'), Symbol.for('f')], [Symbol.for('get-field'), Symbol.for('cache'), Symbol.for('f')]];

/**
 * Set the memoization cache of `f` to `cache`.
 * Returns `f`.
 */
function setCacheX(f: any, cache: any): any {
  f.cache = cache;
  return f;
}

setCacheX.lispSource = [Symbol.for('define'), [Symbol.for('set-cache!'), Symbol.for('f'), Symbol.for('cache')], [Symbol.for('set-field!'), Symbol.for('cache'), Symbol.for('f'), Symbol.for('cache')], Symbol.for('f')];

/**
 * Return a [memoized][w:Memoization] version of the function `f`,
 * where the memoization cache is passed as the last argument.
 *
 * [w:Memoization]: https://en.wikipedia.org/wiki/Memoization
 */
function memoizeWithArg(f: any, arity: any = f.length): any {
  function memoizedF(...args: any[]): any {
    const map: any = args[args.length - 1];
    if ((args.length === arity) && (map instanceof Map)) {
      const args1: any = args.slice(0, -1);
      const path: any = makePath(args1);
      let [value, hasValue]: any[] = mapGetTuple(map, path);
      if (hasValue) {
        return value;
      } else {
        value = f(...args);
        mapSetX(map, path, value);
        return value;
      }
    } else {
      // If no memoization cache was passed, just perform
      // an unmemoized function call.
      return f(...args);
    }
  }
  memoizedF.lispSource = [Symbol.for('define'), [Symbol.for('memoized-f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('map'), [Symbol.for('array-list-last'), Symbol.for('args')]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('args')], Symbol.for('arity')], [Symbol.for('hash?'), Symbol.for('map')]], [Symbol.for('define'), Symbol.for('args1'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('define'), Symbol.for('path'), [Symbol.for('make-path'), Symbol.for('args1')]], [Symbol.for('define-values'), [Symbol.for('value'), Symbol.for('has-value')], [Symbol.for('map-get-tuple'), Symbol.for('map'), Symbol.for('path')]], [Symbol.for('cond'), [Symbol.for('has-value'), Symbol.for('value')], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('value'), [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]], [Symbol.for('map-set!'), Symbol.for('map'), Symbol.for('path'), Symbol.for('value')], Symbol.for('value')]]], [Symbol.for('else'), [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]]]];
  return memoizedF;
}

memoizeWithArg.lispSource = [Symbol.for('define'), [Symbol.for('memoize-with-arg'), Symbol.for('f'), [Symbol.for('arity'), [Symbol.for('get-field'), Symbol.for('length'), Symbol.for('f')]]], [Symbol.for('define'), [Symbol.for('memoized-f'), Symbol.for('.'), Symbol.for('args')], [Symbol.for('define'), Symbol.for('map'), [Symbol.for('array-list-last'), Symbol.for('args')]], [Symbol.for('cond'), [[Symbol.for('and'), [Symbol.for('='), [Symbol.for('array-list-length'), Symbol.for('args')], Symbol.for('arity')], [Symbol.for('hash?'), Symbol.for('map')]], [Symbol.for('define'), Symbol.for('args1'), [Symbol.for('drop-right'), Symbol.for('args'), 1]], [Symbol.for('define'), Symbol.for('path'), [Symbol.for('make-path'), Symbol.for('args1')]], [Symbol.for('define-values'), [Symbol.for('value'), Symbol.for('has-value')], [Symbol.for('map-get-tuple'), Symbol.for('map'), Symbol.for('path')]], [Symbol.for('cond'), [Symbol.for('has-value'), Symbol.for('value')], [Symbol.for('else'), [Symbol.for('set!'), Symbol.for('value'), [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]], [Symbol.for('map-set!'), Symbol.for('map'), Symbol.for('path'), Symbol.for('value')], Symbol.for('value')]]], [Symbol.for('else'), [Symbol.for('apply'), Symbol.for('f'), Symbol.for('args')]]]], Symbol.for('memoized-f')];

/**
 * Make a cache path corresponding to the argument list `args`.
 *
 * The cache path is the given path terminated by a special
 * symbol. This permits paths of any length to be stored in
 * the same cache map; thus, the implementation can handle
 * argument lists of any length.
 */
function makePath(args: any): any {
  return [...args, eof];
}

makePath.lispSource = [Symbol.for('define'), [Symbol.for('make-path'), Symbol.for('args')], [Symbol.for('quasiquote'), [[Symbol.for('unquote-splicing'), Symbol.for('args')], [Symbol.for('unquote'), Symbol.for('eof')]]]];

export {
  eof,
  memoize,
  memoize2,
  memoizeWithArg
};