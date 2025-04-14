"use strict";
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
Object.defineProperty(exports, "__esModule", { value: true });
exports.version = exports.unquoteSym_ = exports.unquoteSplicingSym_ = exports.undefined_ = exports.true_ = exports.quoteSym_ = exports.quasiquoteSym_ = exports.packageName = exports.null_ = exports.license = exports.jsNull_ = exports.false_ = exports.defaultLanguage = void 0;
/**
 * Boolean true value, `#t`.
 *
 * Similar to [`#t` in Racket][rkt:t] and [`t` in Common Lisp][cl:t].
 *
 * [rkt:t]: https://docs.racket-lang.org/reference/reader.html#%28idx._%28gentag._18._%28lib._scribblings%2Freference%2Freference..scrbl%29%29%29
 * [cl:t]: http://clhs.lisp.se/Body/v_t.htm
 */
const true_ = true;
exports.true_ = true_;
/**
 * Boolean false value, `#f`.
 *
 * Similar to [`#f` in Racket][rkt:f] and [`nil` in Common Lisp][cl:nil].
 *
 * [rkt:f]: https://docs.racket-lang.org/reference/reader.html#%28idx._%28gentag._21._%28lib._scribblings%2Freference%2Freference..scrbl%29%29%29
 * [cl:nil]: http://clhs.lisp.se/Body/v_nil.htm
 */
const false_ = false;
exports.false_ = false_;
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
const null_ = [];
exports.null_ = null_;
/**
 * JavaScript's `null` value.
 *
 * The special value [`null` in JavaScript][js:null] is one
 * of JavaScript's primitive values, representing the absence
 * of a value. It is treated as falsy.
 *
 * [js:null]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/null
 */
const jsNull_ = null;
exports.jsNull_ = jsNull_;
/**
 * JavaScript's `undefined` value.
 *
 * The special value [`undefined` in JavaScript][js:undefined] is one
 * of JavaScript's primitive values, representing an undefined value.
 * It is treated as falsy.
 *
 * [js:undefined]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/undefined
 */
const undefined_ = undefined;
exports.undefined_ = undefined_;
/**
 * Symbol for `'`.
 */
const quoteSym_ = Symbol.for('quote');
exports.quoteSym_ = quoteSym_;
/**
 * Symbol for `` ` ``.
 */
const quasiquoteSym_ = Symbol.for('quasiquote');
exports.quasiquoteSym_ = quasiquoteSym_;
/**
 * Symbol for `,`.
 */
const unquoteSym_ = Symbol.for('unquote');
exports.unquoteSym_ = unquoteSym_;
/**
 * Symbol for `,@`.
 */
const unquoteSplicingSym_ = Symbol.for('unquote-splicing');
exports.unquoteSplicingSym_ = unquoteSplicingSym_;
/**
 * Default language to compile to.
 */
const defaultLanguage = 'JavaScript';
exports.defaultLanguage = defaultLanguage;
/**
 * Roselisp package name.
 */
const packageName = 'roselisp';
exports.packageName = packageName;
/**
 * Roselisp version.
 */
const version = '0.0.1';
exports.version = version;
/**
 * Roselisp license.
 */
const license = Symbol.for('MPL-2.0');
exports.license = license;
