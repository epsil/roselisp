// SPDX-License-Identifier: MPL-2.0
// inline-lisp-sources: true
/**
 * # Constants
 *
 * Various constants.
 *
 * ## Description
 *
 * This file defines various constants used by Roselisp.
 *
 * ## License
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

/**
 * Boolean true value, `#t`.
 *
 * Similar to [`#t` in Racket][rkt:t] and [`t` in Common Lisp][cl:t].
 *
 * [rkt:t]: https://docs.racket-lang.org/reference/reader.html#%28idx._%28gentag._18._%28lib._scribblings%2Freference%2Freference..scrbl%29%29%29
 * [cl:t]: http://clhs.lisp.se/Body/v_t.htm
 */
const true_: any = true;

/**
 * Boolean false value, `#f`.
 *
 * Similar to [`#f` in Racket][rkt:f] and [`nil` in Common Lisp][cl:nil].
 *
 * [rkt:f]: https://docs.racket-lang.org/reference/reader.html#%28idx._%28gentag._21._%28lib._scribblings%2Freference%2Freference..scrbl%29%29%29
 * [cl:nil]: http://clhs.lisp.se/Body/v_nil.htm
 */
const false_: any = false;

/**
 * The empty list, `'()`.
 *
 * Similar to [`null` in Racket][rkt:null] and
 * [`nil` in Common Lisp][cl:nil]. However, note that this value
 * is truthy like in Scheme, and not falsy like in Common Lisp.
 *
 * [rkt:null]: https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28quote._~23~25kernel%29._null%29%29
 * [cl:nil]: http://clhs.lisp.se/Body/v_nil.htm
 */
const null_: any = [];

/**
 * JavaScript's `null` value.
 *
 * The special value [`null` in JavaScript][js:null] is one
 * of JavaScript's primitive values, representing the absence
 * of a value. It is treated as falsy.
 *
 * [js:null]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/null
 */
const jsNull_: any = null;

/**
 * JavaScript's `undefined` value.
 *
 * The special value [`undefined` in JavaScript][js:undefined] is one
 * of JavaScript's primitive values, representing an undefined value.
 * It is treated as falsy.
 *
 * [js:undefined]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/undefined
 */
const undefined_: any = undefined;

/**
 * Symbol for `'`.
 */
const quoteSym_: any = Symbol.for('quote');

/**
 * Symbol for `` ` ``.
 */
const quasiquoteSym_: any = Symbol.for('quasiquote');

/**
 * Symbol for `,`.
 */
const unquoteSym_: any = Symbol.for('unquote');

/**
 * Symbol for `,@`.
 */
const unquoteSplicingSym_: any = Symbol.for('unquote-splicing');

/**
 * Default language to compile to.
 */
const defaultLanguage: any = 'JavaScript';

/**
 * Roselisp package name.
 */
const packageName: any = 'roselisp';

/**
 * Roselisp version.
 */
const version: any = '0.0.1';

/**
 * Roselisp license.
 */
const license: any = Symbol.for('MPL-2.0');

export {
  defaultLanguage,
  false_,
  jsNull_,
  license,
  null_,
  packageName,
  quasiquoteSym_,
  quoteSym_,
  true_,
  undefined_,
  unquoteSplicingSym_,
  unquoteSym_,
  version
};